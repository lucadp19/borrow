{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module Typing.Check where

import Prelude hiding ( read )

-- import Typing.Env
import Typing.BetterEnv
import qualified Env as E
import Indices ( Ix(..), block, toTuple )
import Types ( Type(..), RefType(..), Lft(..), shift, (<:), prettyType, isBaseType )
import Syntax ( Deref(..), Term(..), Seq(..) )

import qualified Data.Text as T
import Data.Maybe ( fromJust )
import Data.String ( IsString(..) )

import Control.Monad.State.Class ( MonadState(..), gets, modify )
import Control.Monad.Reader.Class ( MonadReader(..), asks )
import Control.Monad.Reader ( runReader, ReaderT (runReaderT) )
import Control.Monad ( when, unless, (>=>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( MonadError (throwError, catchError), runExcept )
import Env (pushBlock)

copy :: Type -> Bool
copy Unit = True
copy Bool = True
copy Int = True
copy (TRef _ Shr t) = copy t
copy _ = False

clone :: Type -> Bool
clone (TRef _ Uniq _) = False
clone _ = True

deref :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
deref (Deref 0 ix) = do
    Bind _ status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Moved -> throwError "cannot dereference a moved pointer"
        _ -> pure ty
deref (Deref n ix) =
    deref (Deref (n-1) ix) >>= \case
        TRef _ _ ty -> pure ty
        _ -> throwError "cannot dereference a non-pointer type"

mut :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
mut (Deref 0 ix) = do
    Bind mu status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case (mu, status) of
        (Mut, Own) -> pure ty
        (Mut, Moved) -> pure ty
        (Mut, _) -> throwError "cannot assign to a borrowed variable"
        (Imm, _) -> throwError "cannot assign to an immutable variable"
mut p = derefMut p

derefMut :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
derefMut (Deref 0 ix) = do
    Bind _ status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Own -> pure ty
        Borrow _ _ -> throwError "cannot mutably dereference a borrowed reference"
        Moved -> throwError "cannot mutably dereference a moved reference"
derefMut (Deref n ix) =
    derefMut (Deref (n-1) ix) >>= \case
        TRef _ Uniq ty -> pure ty
        TRef _ Shr _ -> throwError "cannot mutably dereference a shared reference"
        _ -> throwError "cannot mutably dereference a non-pointer type"

-- TODO: move this
out :: MonadState s m => m a -> m (a, s)
out m = do
    s <- get
    val <- m
    pure (val, s)

class Typeable t where
    typeof :: forall m. (MonadState TEnv m, MonadError T.Text m) => t -> m Type

instance Typeable Term where
    typeof :: forall m. (MonadState TEnv m, MonadError T.Text m) => Term -> m Type
    -- Literals
    typeof LitUnit = pure Unit
    typeof (LitInt _) = pure Int
    typeof LitTrue = pure Bool
    typeof LitFalse = pure Bool
    typeof (LitString _) = pure String
    -- Variables
    typeof (Var name ix) = do
        Bind _ status ty <- get >>= \env -> case env `E.at` ix of
            Just val -> pure val
            Nothing -> throwError $ invalidIxErr ix
        let n = block ix
        if copy ty
            then case status of
                Own -> case shift ty n of
                    Nothing -> throwError "unreachable lifetime error"
                    Just ty' -> pure ty'
                Borrow Shr _ -> case shift ty n of
                    Nothing -> throwError "unreachable lifetime error"
                    Just ty' -> pure ty'
                Borrow Uniq _ -> throwError $ "cannot use variable `" <> name <> "` as it is currently mutably borrowed"
                Moved -> throwError $ "cannot use variable `" <> name <> "` as it is has already been moved"
            else case status of
                Own -> do
                    maybeModify (poison ix) $ invalidIxErr ix
                    case shift ty n of
                        Nothing -> throwError "unreachable lifetime error"
                        Just ty' -> pure ty'
                Borrow _ _ -> throwError $ "cannot use variable `" <> name <> "`as it is currently borrowed"
                Moved -> throwError $ "cannot use variable `" <> name <> "` as it has already been moved"
    -- Clones
    typeof (Clone p) = get >>= runReaderT (deref p)
    -- Assignment
    typeof (Assign p t) = do
        ty <- get >>= runReaderT (mut p)
        ty' <- typeof t
        if ty' <: ty -- type of the RHS must be a subtype of the LHS's type
            then pure Unit
            else throwError $ "type mismatch: expected `" <> prettyType ty <> "` but got `" <> prettyType ty' <> "`" 
    -- Shared reference
    typeof (Ref name ix) = do
        Bind _ status ty <- get >>= \env -> case env `E.at` ix of
            Just val -> pure val
            Nothing -> throwError $ invalidIxErr ix
        case status of
            Own -> TRef (Loc 0) Shr ty <$ maybeModify (borrowShr ix) (invalidIxErr ix)
            Borrow Shr _ -> pure $ TRef (Loc 0) Shr ty
            Borrow Uniq _ -> throwError $ "cannot create shared reference to `" <> name <> "` as it is currently mutably borrowed"
            Moved -> throwError $ "cannot create shared reference to `" <> name <> "` as it has been moved"
    -- Unique reference
    typeof (RefMut name ix) = do
        Bind mu status ty <- get >>= \env -> case env `E.at` ix of
            Just val -> pure val
            Nothing -> throwError $ invalidIxErr ix
        case (mu, status) of
            (Mut, Own) -> TRef (Loc 0) Uniq ty <$ maybeModify (borrowUniq ix) (invalidIxErr ix)
            (Mut, Borrow _ _) -> throwError $ "cannot create mutable reference to `" <> name <> "` as it is already borrowed"
            (Mut, Moved) -> throwError $ "cannot create mutable reference to `" <> name <> "` as it has already been moved"
            (Imm, _) -> throwError $ "cannot create mutable reference to `" <> name <> "`as it has not been declared as `mut`" 
    -- If
    typeof (IfThenElse cond t1 t2) = do
        tyCond <- typeof cond
        when (tyCond /= Bool) $ throwError $ "type mismatch: expected `bool` but got `" <> prettyType tyCond <> "`"
        env <- get
        (ty1, env') <- out $ typeof t1
        (ty2, env'') <- put env *> out (typeof t2)
        if ty1 == ty2 && env' == env''
            then pure ty1
            else throwError "types and environment of if branches must be equal"
    -- Block
    typeof (Block seqn) = do
        ty <- modify pushBlock *> typeof seqn
        modify $ fromJust . E.popBlock . endlft
        case shift ty (-1) of
            Just ty' -> pure ty'
            Nothing -> throwError "cannot return value as it does not live long enough"
    -- Function abstraction
    typeof (Fn n (params, outTy) body) = do
        wf n $ TFn n params outTy -- checking that the type is valid
        env <- get
        let fnEnv = fromJust $ buildEnv (E.Env [E.Block []]) params
        ty <- put fnEnv *> typeof body
        if ty <: outTy
            then TFn n params outTy <$ put env
            else throwError $ "type mismatch: expected `" <> prettyType outTy <> "` but got `" <> prettyType ty <> "`"
      where
        wf :: Int -> Type -> m ()
        wf n ty | isBaseType ty = pure ()
        wf n (TRef (LftVar 0 k) _ ty) 
            = if k < n then wf n ty 
                else throwError "function argument contains an unspecified generic lifetime"
        wf n (TRef {}) = throwError "function argument contains a non-generic lifetime"
        wf _ (TFn n' params' outTy') = mapM (wf n') params' *> wf n' outTy'

        buildEnv :: TEnv -> [Type] -> Maybe TEnv
        buildEnv env [] = pure env
        buildEnv env (ty:tys) = insert Imm ty env >>= \env' -> buildEnv env' tys
    -- Function application
    typeof (Appl fn lfts terms) = typeof fn >>= \case
        -- fn has function type
        TFn n params outTy -> do
            -- lifetime list's length must match number of expected lifetime arguments
            when (length lfts /= n) $ throwError $ "expected " <> T.pack (show n) 
                    <> " lifetimes arguments, but got " <> (T.pack . show . length) lfts
            -- argument list's length must match number of expected arguments
            when (length terms /= length params) $ throwError $ "expected " <> (T.pack . show . length) params
                    <> " arguments, but got " <> (T.pack . show . length) terms
            -- instantiating generic lifetimes
            let params' = subst lfts <$> params
            termsTys <- mapM typeof terms
            unless (and $ zipWith (<:) termsTys params') $ throwError
                "type mismatch in function application: argument type does not match expected type"
            pure $ subst lfts outTy
        -- wrong type
        t -> throwError $ "type mismatch: expected function type, but got `" <> prettyType t <> "`"
      where
        subst :: [Lft] -> Type -> Type
        subst list = \case
            TRef (LftVar 0 k) ref ty -> TRef (list !! k) ref $ subst list ty
            ty -> ty 

instance Typeable Seq where
    typeof (Let _ t seqn) = do
        ty <- typeof t
        modify $ fromJust . insert Imm ty
        typeof seqn
    typeof (LetMut _ t seqn) = do
        ty <- typeof t
        modify $ fromJust . insert Mut ty
        typeof seqn
    typeof (Seq t seqn) = typeof t *> typeof seqn
    typeof (Final t) = typeof t

maybeModify :: (MonadError e m, MonadState s m) => (s -> Maybe s) -> e -> m ()
maybeModify f err = do
    env <- get
    case f env of
        Nothing -> throwError err
        Just env' -> put env' *> pure ()

invalidIxErr :: Ix -> T.Text
invalidIxErr ix = "invalid index: no value at position " <> (fromString . show . toTuple) ix