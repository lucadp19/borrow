{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module Language.Borrow.Interpreter.Eval 
    ( Tmp
    , Evaluable(..)
    , fullEval
    )
where

import Prelude hiding ( read )

import qualified Language.Borrow.Env as E
import qualified Language.Borrow.Interpreter.Store as S
import qualified Language.Borrow.Interpreter.Heap as H
import Language.Borrow.Interpreter.Values ( Value(..) )

import Language.Borrow.Indices ( Ix(..), toTuple, (<+>) )
import Language.Borrow.Syntax ( Term(..), Seq(..), Deref(..), Block(..), BinaryOp(..), UnaryOp(..), prettyBin )

import Control.Monad.State.Class ( MonadState(..), modify )
import Control.Monad.Reader.Class ( MonadReader(..), asks )
import Control.Monad.Reader ( runReaderT )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Maybe ( fromJust )
import Data.Functor ( ($>) )

data Tmp = Tmp deriving (Show)

-- | Clones the value obtained by dereferencing the given dereferece-term.
clone :: (MonadState (S.Store Value) m, MonadError T.Text m) 
    => Deref                    -- ^ The dereference
    -> m (Either Tmp Value)     -- ^ The obtained value
clone p@(Deref name n ix) = get >>= \store -> case deref p store of
    Nothing -> throwError $ "cannot dereference `" <> name <> "` for " <> (T.pack . show) n <> " times"
    Just (totalIx, val) -> case val of
        VPtr loc -> do
            val' <- maybe (throwError "could not find dereferenced value in the heap") pure $ S.heapLookup loc store
            modify $ fromJust . S.heapInsert val' -- there is a value in the env, hence we can insert values in tmp
            pure $ Left Tmp
        VRef ix' -> pure . Right . VRef $ ix' <+> totalIx
        val -> pure $ Right val

-- | Assigns the given value to the given dereference-term.
assign :: (MonadState (S.Store Value) m, MonadError T.Text m) 
    => Deref    -- ^ The pointer
    -> Value    -- ^ The value to be assigned
    -> m ()
assign p@(Deref name n ix) val = get >>= \store -> case deref p store of
    Nothing -> throwError $ "cannot dereference the variable at index " <> (T.pack . show . toTuple) ix <> 
        " for " <> (T.pack . show) n <> " times"
    Just (totalIx, oldVal) -> case oldVal of
        VPtr loc -> do
            case S.pushTmp loc store of
                Nothing -> throwError "unexpected error: cannot insert value in temporary stack"
                Just store' -> case val `shiftVal` totalIx of
                    Nothing -> throwError "unexpected error: cannot update value in environment as it would result in negative indices" 
                    Just val' -> put $ S.adjust (const val') totalIx store'
        _ -> do
            case val `shiftVal` totalIx of
                Nothing -> throwError "unexpected error: cannot update value in environment as it would result in negative indices" 
                Just val' -> put $ S.adjust (const val') totalIx store
  where
    -- | Negatively shifts a value by the given index.
    shiftVal :: Value -> Ix -> Maybe Value
    shiftVal (VRef ix) off = VRef <$> ix `ixAssignSub` off
    shiftVal v _ = Just v

    -- | Negatively shifts the first index by the second index.
    ixAssignSub :: Ix -> Ix -> Maybe Ix
    (Ix n k) `ixAssignSub` (Ix n' k')
        | n > n'            = Just $ Ix (n - n') k
        | n == n' && n > 0  = Just $ Ix n k
        | n == n' && n == 0 = Just $ Ix 0 $ k - k' - 1
        | otherwise         = Nothing 

-- | Dereferences a pointer and gets the index of the pointed variable and its (non-shifted) value.
deref :: Deref -> S.Store Value -> Maybe (Ix, Value)
deref (Deref name 0 ix) store = store `S.at` ix >>= \val -> Just (ix, val)
deref (Deref name n ix) store = store `S.at` ix >>= \case
    VRef ix' -> deref (Deref name (n-1) $ ix' <+> ix) store
    _        -> Nothing

-- | Reads the inner value pointed by the given value.
read :: (MonadReader (S.Store Value) m, MonadError T.Text m) 
    => Value 
    -> m Value
read = \case
    VRef ix -> asks (`S.at` ix) >>= \case
        Nothing -> throwError "variable is not present in the enviroment"
        Just (VRef ix') -> read $ VRef $ ix' <+> ix
        Just val -> read val
    VPtr loc -> asks (S.heapLookup loc) >>= \case
        Nothing -> throwError "valid pointer has been removed from the heap"
        Just val -> pure val
    VMoved -> throwError "cannot read moved value!"
    val -> pure val

-- | A typeclass for all evaluable syntactic terms.
class Evaluable t where
    eval :: forall m. (MonadState (S.Store Value) m, MonadError T.Text m, MonadIO m) 
        => t                        -- ^ The term to be evaluated
        -> m (Either Tmp Value)     -- ^ The resulting value

-- | Fully evaluates a term, reducing eventual @Tmp@s.
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
        store <- get
        case S.heapInsert (VString str) store of
            Nothing -> throwError "cannot evaluate string literal as there is no block to insert it into"
            Just store' -> put store' $> Left Tmp
    -- Variables
    eval (Var name ix) = do
        get >>= \store -> case store `S.at` ix of
            Nothing -> throwError $ "cannot evaluate `" <> name <> "` as it isn't present in the environment" 
            Just VMoved -> throwError "use of moved value"
            Just (VPtr loc) -> do
                modify $ S.adjust (const VMoved) ix 
                modify $ fromJust . S.pushTmp loc -- there is at least a value, hence at least a block
                pure $ Left Tmp
            Just (VRef ix') -> pure . Right . VRef $ ix' <+> ix
            Just val -> pure $ Right val
    -- Equality
    eval (Binary Eq t1 t2) = do
        v1 <- fullEval t1
        v2 <- fullEval t2
        case v1 of
            VPtr loc -> modify $ fromJust . S.pushTmp loc
            _ -> pure ()
        case v2 of
            VPtr loc -> modify $ fromJust . S.pushTmp loc
            _ -> pure ()
        pointedVal1 <- get >>= runReaderT (read v1)
        pointedVal2 <- get >>= runReaderT (read v2)
        evalBin Eq pointedVal1 pointedVal2
    --  Binary operation
    eval (Binary op t1 t2) = do
        v1 <- fullEval t1
        v2 <- fullEval t2
        evalBin op v1 v2
    -- Unary operations
    eval (Unary Neg t1) = eval t1 >>= \case
        Right (VInt n) -> pure . Right . VInt $ -n
        _ -> throwError "can only apply `-` to integers"
    eval (Unary Not t1) = eval t1 >>= \case
        Right (VBool b) -> pure . Right . VBool $ not b
        _ -> throwError "can only apply `!` to booleans"
    -- Println
    eval (Println terms) = printAll terms $> Right VUnit
      where
        printAll :: [Term] -> m ()
        printAll [] = liftIO $ putStrLn ""
        printAll (t:ts) = printSingle t *> printAll ts

        printSingle :: Term -> m ()
        printSingle t = do
            val <- fullEval t
            case val of
                VPtr loc -> modify $ fromJust . S.pushTmp loc
                _ -> pure ()
            pointedVal <- get >>= runReaderT (read val) 
            case pointedVal of
                VInt n      -> liftIO $ putStr $ show n
                VBool b     -> liftIO $ putStr $ show b
                VString s   -> liftIO $ TIO.putStr s
                VCode _     -> liftIO $ putStr "<fn-pointer>"
                _           -> throwError "unreachable possibility"
    -- Clone
    eval (Clone p) = clone p
    -- References
    eval (Ref (Deref _ 0 ix)) = pure . Right $ VRef ix
    eval (RefMut (Deref _ 0 ix)) = pure . Right $ VRef ix
    -- Reborrows
    eval (Ref p) = get >>= \store -> case deref p store of
        Just (ix', _)   -> pure . Right . VRef $ ix'
        Nothing         -> throwError "cannot dereference value in reborrow"
    eval (RefMut p) = get >>= \store -> case deref p store of
        Just (ix', _)   -> pure . Right . VRef $ ix'
        Nothing         -> throwError "cannot dereference value in reborrow"
    -- Assignment
    eval (Assign p t) = do
        value <- fullEval t
        assign p value
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
        modify $ fromJust . S.pushTmp loc -- discarding the used pointer
        body <- get >>= \store -> case S.heapLookup loc store of
            Nothing -> throwError "error: cannot find location in heap"
            Just (VCode seqn) -> pure seqn
            Just _ -> throwError "error: expected fuction closure"
        args <- shiftVals <$> evalArgs terms
        modify $ S.pushBlockWithArgs args
        res <- fullEval body >>= unshift
        modify $ fromJust . S.popBlock getLocations -- safe as we put a new block just before
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
        modify $ fromJust . S.popBlock getLocations
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

-- | Gets all the pointers from a list of values.
getLocations :: [Value] -> [H.Location]
getLocations [] = []
getLocations (VPtr l:vs) = l : getLocations vs
getLocations (v:vs) = getLocations vs

-- | Evaluates a binary operation.
evalBin :: (MonadState (S.Store Value) m, MonadError T.Text m) 
        => BinaryOp 
        -> Value 
        -> Value 
        -> m (Either Tmp Value)
-- Integer arithmetic
evalBin Sum  (VInt n) (VInt m)  = pure . Right . VInt $ n + m
evalBin Prod (VInt n) (VInt m)  = pure . Right . VInt $ n * m
evalBin Sub  (VInt n) (VInt m)  = pure . Right . VInt $ n - m
evalBin Div  (VInt n) (VInt m)  = if m == 0 
    then throwError "cannot divide by zero"
    else pure . Right . VInt $ n `div` m
-- Integer comparisons
evalBin Less (VInt n) (VInt m)  = pure . Right . VBool $ n < m
evalBin More (VInt n) (VInt m)  = pure . Right . VBool $ n > m
evalBin Leq  (VInt n) (VInt m)  = pure . Right . VBool $ n <= m
evalBin Geq  (VInt n) (VInt m)  = pure . Right . VBool $ n >= m
-- Boolean operations
evalBin And (VBool p) (VBool q) = pure . Right . VBool $ p && q
evalBin Or  (VBool p) (VBool q) = pure . Right . VBool $ p || q
-- Equality
evalBin Eq v1 v2                = pure . Right . VBool $ v1 == v2
-- Concatenation
evalBin Concat (VPtr l1) (VPtr l2) = do
    store <- get
    case (S.heapLookup l1 store, S.heapLookup l2 store) of
        (Just (VString s1), Just (VString s2)) -> modify $ S.heapAdjust (const (VString $ s1 <> s2)) l1
        (_, _) -> throwError "heap error: could not get the strings to concatenate"
    modify $ fromJust . S.pushTmp l2
    modify $ fromJust . S.pushTmp l1
    pure . Left $ Tmp
-- Error
evalBin op _ _ = throwError $ "values of a wrong type were used as argument of `" <> prettyBin op <> "`"