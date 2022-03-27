{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval.Eval where

import qualified Eval.State as S

import qualified Env as E
import Indices ( Ix(..), toTuple, (<+>), ixSub)
import qualified Eval.Heap as H
import qualified Data.Text as T
import Syntax (Term(..), Seq(..), Deref(..))
import Control.Monad.State.Class ( MonadState(..), modify )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Maybe (fromJust)
import Data.Functor ( ($>) )

data Value
    = VUnit
    | VInt Int
    | VBool Bool
    | VRef Ix
    | VPtr H.Location
    | VString T.Text
    | VCode Term
    | VMoved

instance Show Value where
    show VUnit = "()"
    show (VInt n) = show n
    show (VBool b) = show b
    show (VRef ix) = "ref " <> show (toTuple ix)
    show (VPtr _) = "<heap ptr>"
    show (VString str) = show str
    show (VCode _) = "<code>"
    show VMoved = "MOVED" 

data Tmp = Tmp deriving (Show)

clone :: (MonadState (S.State Value) m) => Int -> Ix -> m (Maybe (Either Tmp Value))
clone n ix = do
    state <- get
    case deref n ix (S.env state) of
        Nothing -> pure Nothing
        Just (totalIx, val) -> case val of
            VPtr loc -> case H.lookup (S.heap state) loc of
                Just val -> do 
                    modify $ fromJust . S.heapInsert val
                    pure . Just . Left $ Tmp
                Nothing -> pure Nothing
            VRef ix' -> pure . Just . Right . VRef $ ix' <+> totalIx
            val -> pure . Just . Right $ val

assign :: (MonadState (S.State Value) m, MonadIO m) => Int -> Ix -> Value -> m (Maybe ())
assign n ix val = do
    s <- get
    case deref n ix (S.env s) of
        Nothing -> pure Nothing
        Just (totalIx, oldVal) -> case oldVal of
            VPtr loc -> do
                liftIO $ print $ "total ix: " <> show (toTuple totalIx) 
                    <> " OldValue: " <> show oldVal
                    <> " Value: " <> show val
                modify $ fromJust . S.pushTmp loc
                modify $ fromJust . S.adjust (const $ update totalIx val) totalIx
                pure $ Just ()
            _ -> do
                liftIO $ print $ "total ix: " <> show (toTuple totalIx) 
                    <> " OldValue: " <> show oldVal
                    <> " Value: " <> show val
                modify $ fromJust . S.adjust (const $ update totalIx val) totalIx
                pure $ Just ()
  where
    update :: Ix -> Value -> Value
    update ix val = fromJust $ shiftVal val ix

    shiftVal :: Value -> Ix -> Maybe Value
    shiftVal (VRef ix) off = VRef <$> ix `ixSub` off
    shiftVal v _ = Just v

deref :: Int -> Ix -> E.Env Value -> Maybe (Ix, Value)
deref n ix = go n ix ix
  where
    go :: Int -> Ix -> Ix -> E.Env Value -> Maybe (Ix, Value)
    go 0 ix off env = env `E.at` ix >>= \val -> Just (off, val)
    go n (Ix 0 0) off (E.Env env) = case env of
        [] -> Nothing 
        (E.Block b):bs -> case b of
            (VRef ix'):xs -> go (n-1) ix' (ix' <+> off) $ E.Env $ E.Block xs:bs
            _ -> Nothing
    go n (Ix 0 k) off (E.Env env) = case env of
        [] -> Nothing
        (E.Block b):bs -> case b of
            x:xs -> go n (Ix 0 $ k-1) off $ E.Env $ E.Block xs:bs
            _ -> Nothing
    go n (Ix m k) off (E.Env env) = case env of
        [] -> Nothing
        b:bs -> go n (Ix (m-1) k) off $ E.Env bs


class Evaluable t where
    eval :: (MonadState (S.State Value) m, MonadError T.Text m, MonadIO m) => t -> m (Either Tmp Value)

fullEval :: (Evaluable t, MonadState (S.State Value) m, MonadError T.Text m, MonadIO m) => t -> m Value
fullEval t = eval t >>= \case
    Left Tmp -> state S.popTmp >>= \case
        Nothing -> throwError "no temporary value to pop"
        Just loc -> pure $ VPtr loc
    Right val -> pure val

instance Evaluable Term where
    -- Base values
    eval LitUnit = pure $ Right VUnit
    eval (LitInt n) = pure $ Right $ VInt n
    eval LitTrue = pure $ Right $ VBool True
    eval LitFalse = pure $ Right $ VBool False
    eval (LitString str) = do
        state <- get
        modify $ fromJust . S.heapInsert (VString str)
        pure $ Left Tmp
    -- Variables
    eval (Var _ ix) = do
        get >>= \state -> case state `S.at` ix of
            Nothing -> throwError "error: no value"
            Just VMoved -> throwError "error: use of moved value"
            Just (VPtr loc) -> do
                modify $ fromJust . S.adjust (const VMoved) ix
                modify $ fromJust . S.pushTmp loc
                pure $ Left Tmp
            Just (VRef ix') -> pure . Right . VRef $ ix' <+> ix -- CHECK
            Just val -> pure $ Right val
    -- Clone
    eval (Clone (Deref n ix)) = clone n ix >>= \case
        Nothing -> throwError "error!"
        Just val -> pure val
    -- References
    eval (Ref _ ix) = pure . Right $ VRef ix
    eval (RefMut _ ix) = pure . Right $ VRef ix
    -- Assignment
    eval (Assign (Deref n ix) t) = do
        value <- fullEval t
        assign n ix value >>= \case
            Nothing -> throwError "ERROR!"
            Just () -> pure . Right $ VUnit
    -- Conditional
    eval (IfThenElse cond t1 t2) = do
        val <- fullEval cond
        case val of
            VBool True -> eval t1
            VBool False -> eval t2
            _ -> throwError "wrong type"
    -- Block
    eval (Block seqn) = do
        modify S.pushBlock
        val <- fullEval seqn
        get >>= \s -> liftIO $ print (S.env s)
        modify $ fromJust . popBlock
        -- decrease block in ref by one!!
        pure . Right $ val
    eval _ = undefined

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

popBlock :: S.State Value -> Maybe (S.State Value)
popBlock s = case (S.tmp s, S.env s) of
    (tmpBlock : tmp', E.Env (E.Block envBlock : env')) -> 
        let heap' = batchDelete (S.heap s) tmpBlock 
            heap'' = batchDelete heap' $ getPtrs envBlock in
        Just $ s { S.tmp = tmp', S.env = E.Env env', S.heap = heap'' }
    _ -> Nothing
  where
    batchDelete :: H.Heap a -> [H.Location] -> H.Heap a 
    batchDelete h [] = h
    batchDelete h (l:ls) = batchDelete (H.delete h l) ls

    getPtrs :: [Value] -> [H.Location]
    getPtrs [] = []
    getPtrs (VPtr l:vs) = l : getPtrs vs
    getPtrs (v:vs) = getPtrs vs