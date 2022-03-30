{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module Language.BorrowLang.Interpreter.Eval where

import qualified Language.BorrowLang.Env as E
import qualified Language.BorrowLang.Interpreter.Store as S
import qualified Language.BorrowLang.Interpreter.Heap as H
import Language.BorrowLang.Interpreter.Values ( Value(..) )

import Language.BorrowLang.Indices ( Ix(..), toTuple, (<+>) )
import Language.BorrowLang.Syntax ( Term(..), Seq(..), Deref(..), Block(..) )

import Control.Monad.State.Class ( MonadState(..), modify )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )

import qualified Data.Text as T
import Data.Maybe ( fromJust )
import Data.Functor ( ($>) )

data Tmp = Tmp deriving (Show)

clone :: (MonadState (S.Store Value) m, MonadError T.Text m) => Int -> Ix -> m (Either Tmp Value)
clone n ix = do
    store <- get
    case deref n ix (S.env store) of
        Nothing -> throwError $ "cannot dereference the variable at index " <> (T.pack . show . toTuple) ix <> 
            " for " <> (T.pack . show) n <> " times"
        Just (totalIx, val) -> case val of
            VPtr loc -> do
                val' <- maybe (throwError "could not find dereferenced value in the heap") pure $ S.heapLookup loc store
                modify $ fromJust . S.heapInsert val'
                pure $ Left Tmp
            VRef ix' -> pure $ Right $ VRef $ ix' <+> totalIx
            val -> pure $ Right val

assign :: (MonadState (S.Store Value) m, MonadIO m, MonadError T.Text m) => Int -> Ix -> Value -> m ()
assign n ix val = do
    s <- get
    case deref n ix (S.env s) of
        Nothing -> throwError $ "cannot dereference the variable at index " <> (T.pack . show . toTuple) ix <> 
            " for " <> (T.pack . show) n <> " times"
        Just (totalIx, oldVal) -> case oldVal of
            VPtr loc -> do
                -- liftIO $ print $ "total ix: " <> show (toTuple totalIx) 
                --     <> " OldValue: " <> show oldVal
                --     <> " Value: " <> show val
                get >>= \s -> case S.pushTmp loc s of
                    Nothing -> throwError "unexpected error: cannot insert value in temporary stack"
                    Just s' -> case val `shiftVal` totalIx of
                        Nothing -> throwError "unexpected error: cannot update value in environment as it would result in negative indices" 
                        Just val' -> put $ S.adjust (const val') totalIx s'
            _ -> do
                -- liftIO $ print $ "total ix: " <> show (toTuple totalIx) 
                --     <> " OldValue: " <> show oldVal
                --     <> " Value: " <> show val
                get >>= \s -> case val `shiftVal` totalIx of
                    Nothing -> throwError "unexpected error: cannot update value in environment as it would result in negative indices" 
                    Just val' -> put $ S.adjust (const val') totalIx s
  where
    shiftVal :: Value -> Ix -> Maybe Value
    shiftVal (VRef ix) off = VRef <$> ix `ixAssignSub` off
    shiftVal v _ = Just v

    ixAssignSub :: Ix -> Ix -> Maybe Ix
    (Ix n k) `ixAssignSub` (Ix n' k')
        | n > n'            = Just $ Ix (n - n') k
        | n == n' && n > 0  = Just $ Ix n k
        | n == n' && n == 0 && k > k'  
                            = Just $ Ix 0 $ k - k' - 1
        | otherwise         = Nothing 

deref :: Int -> Ix -> E.Env Value -> Maybe (Ix, Value)
deref n ix = go n ix ix
  where
    go :: Int -> Ix -> Ix -> E.Env Value -> Maybe (Ix, Value)
    go 0 ix off = \env -> env `E.at` ix >>= \val -> Just (off, val)
    go n (Ix 0 0) off = \case
        [] -> Nothing 
        b:bs -> case b of
            (VRef ix'):xs -> go (n-1) ix' (ix' <+> off) $ xs:bs
            _ -> Nothing
    go n (Ix 0 k) off = \case
        [] -> Nothing
        b:bs -> case b of
            x:xs -> go n (Ix 0 $ k-1) off $ xs:bs
            _ -> Nothing
    go n (Ix m k) off = \case
        [] -> Nothing
        b:bs -> go n (Ix (m-1) k) off bs

class Evaluable t where
    eval :: forall m. (MonadState (S.Store Value) m, MonadError T.Text m, MonadIO m) => t -> m (Either Tmp Value)

fullEval :: (Evaluable t, MonadState (S.Store Value) m, MonadError T.Text m, MonadIO m) => t -> m Value
fullEval t = eval t >>= \case
    Left Tmp -> state S.popTmp >>= \case
        Nothing -> throwError "no temporary value to pop"
        Just loc -> pure $ VPtr loc
    Right val -> pure val

instance Evaluable Term where
    eval :: forall m. (MonadState (S.Store Value) m, MonadError T.Text m, MonadIO m) => Term -> m (Either Tmp Value)
    -- Base values
    eval LitUnit = pure $ Right VUnit
    eval (LitInt n) = pure $ Right $ VInt n
    eval LitTrue = pure $ Right $ VBool True
    eval LitFalse = pure $ Right $ VBool False
    eval (LitString str) = do
        statstoree <- get
        modify $ fromJust . S.heapInsert (VString str)
        pure $ Left Tmp
    -- Variables
    eval (Var _ ix) = do
        get >>= \store -> case store `S.at` ix of
            Nothing -> throwError $ "cannot evaluate variable as there is no value at index " 
                <> (T.pack . show . toTuple) ix <> " in the environment"
            Just VMoved -> throwError "error! use of moved value"
            Just (VPtr loc) -> do
                modify $ S.adjust (const VMoved) ix 
                modify $ fromJust . S.pushTmp loc -- there is at least a value, hence at least a block
                pure $ Left Tmp
            Just (VRef ix') -> pure . Right . VRef $ ix' <+> ix
            Just val -> pure $ Right val
    -- Clone
    eval (Clone (Deref n ix)) = clone n ix
    -- References
    eval (Ref _ ix) = pure . Right $ VRef ix
    eval (RefMut _ ix) = pure . Right $ VRef ix
    -- Assignment
    eval (Assign (Deref n ix) t) = do
        value <- fullEval t
        assign n ix value
        pure $ Right VUnit
    -- Conditional
    eval (IfThenElse cond t1 t2) = do
        val <- fullEval cond
        case val of
            VBool True -> eval t1
            VBool False -> eval t2
            _ -> throwError "condition in if expression must evaluate to either `True` or `False`"
    -- Block
    eval (TBlock block) = eval block
    -- Function abstraction
    eval (Fn _ _ (Block seqn)) = do
        store <- get
        case S.heapInsert (VCode seqn) store of
            Nothing -> throwError "unexpected error: cannot insert function closure in heap"
            Just store' -> put store' $> Left Tmp
    -- Function application
    eval (Appl fn _ terms) = do
        loc <- fullEval fn >>= \case
            VPtr loc -> pure loc
            _ -> throwError "function value was not a heap pointer" 
        body <- get >>= \store -> case S.heapLookup loc store of
            Nothing -> throwError "error: cannot find location in heap"
            Just (VCode seqn) -> pure seqn
            Just _ -> throwError "error: expected fuction closure"
        args <- shiftVals <$> evalArgs terms
        modify $ S.pushBlockWithArgs args
        res <- fullEval body >>= unshift
        modify $ fromJust . S.popBlock cleanHeap -- safe as we put a new block just before
        case res of
            VPtr loc -> do
                modify $ fromJust . S.pushTmp loc
                pure . Left $ Tmp
            v -> pure . Right $ v
      where
        evalArgs :: [Term] -> m [Value]
        evalArgs [] = pure []
        evalArgs (t:ts) = do
            v <- fullEval t
            vs <- evalArgs ts
            pure $ v:vs

        shiftVals :: [Value] -> [Value]
        shiftVals [] = []
        shiftVals (v:vs) = case v of
            VRef ix -> VRef (ix <> Ix 1 0) : shiftVals vs
            v -> v : shiftVals vs

        unshift :: Value -> m Value
        unshift = \case
            VRef (Ix n k) -> if n > 0 
                then pure . VRef $ Ix (n-1) k
                else throwError "error: could not return value as it did not live long enough"
            v -> pure v

instance Evaluable Block where
    eval (Block seqn) = do
        modify S.pushBlock
        val <- fullEval seqn
        -- get >>= \s -> liftIO $ print (S.env s)
        modify $ fromJust . S.popBlock cleanHeap
        val' <- case val of
            VRef (Ix n k) -> if n > 0 
                then pure . VRef $ Ix (n-1) k
                else throwError "error: could not return value as it did not live long enough"
            _ -> pure val
        pure . Right $ val'

instance Evaluable Seq where
    eval = \case
        Let _ t seqn -> do
            val <- fullEval t
            modify $ fromJust . S.insert val
            eval seqn 
        LetMut _ t seqn -> do
            val <- fullEval t
            modify $ fromJust . S.insert val
            eval seqn 
        Seq t seqn -> eval t *> eval seqn
        Final t -> eval t

-- | Removes from the heap all the locations contained in the first two lists,
-- either as pure locations or as contents of a @VPtr@ value.
cleanHeap :: [H.Location] -> [Value] -> H.Heap a -> H.Heap a
cleanHeap locs vals = H.deleteMultiple (locs <> getLocations vals)
  where
    getLocations :: [Value] -> [H.Location]
    getLocations [] = []
    getLocations (VPtr l:vs) = l : getLocations vs
    getLocations (v:vs) = getLocations vs