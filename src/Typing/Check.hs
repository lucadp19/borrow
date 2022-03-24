{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Typing.Check where

import Prelude hiding ( read )

import Typing.Env
import Indices ( block )
import Types ( Type(..), RefType(..), Lft(..), shift, (<:), prettyType )
import Syntax ( Deref(..), Term(..), Seq(..) )

import qualified Data.Text as T
import Data.Maybe ( fromJust )

import Control.Monad.State.Class ( MonadState(..), gets, modify )
import Control.Monad.Reader.Class ( MonadReader(..), asks )
import Control.Monad.Reader ( runReader, ReaderT (runReaderT), MonadTrans (lift) )
import Control.Monad ( when, unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( MonadError (throwError), runExcept )

copy :: Type -> Bool
copy Unit = True
copy Bool = True
copy Int = True
copy (TRef _ Shr t) = copy t
copy _ = False

clone :: Type -> Bool
clone (TRef _ Uniq _) = False
clone _ = True

deref :: (MonadReader Env m, MonadError T.Text m) => Deref -> m Type
deref (Deref 0 ix) = do
    Bind _ status ty <- asks (`at` ix)
    case status of
        Moved -> throwError "cannot dereference a moved pointer"
        _ -> pure ty
deref (Deref n ix) =
    deref (Deref (n-1) ix) >>= \case
        TRef _ _ ty -> pure ty
        _ -> throwError "cannot dereference a non-pointer type"

mut :: (MonadReader Env m, MonadError T.Text m) => Deref -> m Type
mut (Deref 0 ix) = do
    Bind mu status ty <- asks (`at` ix)
    case (mu, status) of
        (Mut, Own) -> pure ty
        (Mut, Moved) -> pure ty
        (Mut, _) -> throwError "cannot assign to a borrowed variable"
        (Imm, _) -> throwError "cannot assign to an immutable variable"
mut p = derefMut p

derefMut :: (MonadReader Env m, MonadError T.Text m) => Deref -> m Type
derefMut (Deref 0 ix) = do
    Bind _ status ty <- asks (`at` ix)
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
    typeof :: (MonadState Env m, MonadError T.Text m, MonadIO m) => t -> m Type

instance Typeable Term where
    -- Literals
    typeof LitUnit = pure Unit
    typeof (LitInt _) = pure Int
    typeof LitTrue = pure Bool
    typeof LitFalse = pure Bool
    typeof (LitString _) = pure String
    -- Variables
    typeof (Var name ix) = do
        Bind _ status ty <- gets (`at` ix)
        let n = block ix
        if copy ty
            then case status of
                Own -> case shift ty n of
                    Nothing -> throwError "unexpected lifetime error"
                    Just ty' -> pure ty'
                Borrow Shr _ -> case shift ty n of
                    Nothing -> throwError "unexpected lifetime error"
                    Just ty' -> pure ty'
                Borrow Uniq _ -> throwError $ "cannot use variable `" <> name <> "` as it is currently mutably borrowed"
                Moved -> throwError $ "cannot use variable `" <> name <> "` as it is has already been moved"
            else case status of
                Own -> do
                    modify $ poison ix
                    case shift ty n of
                        Nothing -> throwError "unexpected lifetime error"
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
        Bind _ status ty <- gets (`at` ix)
        case status of
            Own -> TRef (Loc 0) Shr ty <$ modify (borrowShr ix)
            Borrow Shr _ -> pure $ TRef (Loc 0) Shr ty
            Borrow Uniq _ -> throwError $ "cannot create shared reference to `" <> name <> "` as it is currently mutably borrowed"
            Moved -> throwError $ "cannot create shared reference to `" <> name <> "` as it has been moved"
    -- Unique reference
    typeof (RefMut name ix) = do
        Bind mu status ty <- gets (`at` ix)
        case (mu, status) of
            (Mut, Own) -> TRef (Loc 0) Uniq ty <$ modify (borrowUniq ix) 
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
        ty <- typeof seqn <* modify ([] :)
        modify $ tail . endlft
        case shift ty (-1) of
            Just ty' -> pure ty'
            Nothing -> throwError "cannot return value as it does not live long enough"
    -- Function abstraction
    typeof (Fn n (params, outTy) body) = do
        env <- get
        let fnEnv = buildEnv [[]] params
        ty <- put fnEnv *> typeof body
        if ty <: outTy
            then TFn n params outTy <$ put env
            else throwError $ "type mismatch: expected `" <> prettyType outTy <> "` but got `" <> prettyType ty <> "`"
      where
        buildEnv :: Env -> [Type] -> Env
        buildEnv env [] = env
        buildEnv env (ty:tys) = buildEnv (insert Imm ty env) tys
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
        modify $ insert Imm ty
        typeof seqn
    typeof (LetMut _ t seqn) = do
        ty <- typeof t
        modify $ insert Mut ty
        typeof seqn
    typeof (Seq t seqn) = typeof t *> typeof seqn
    typeof (Final t) = typeof t
