{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Typing.Check where

import Prelude hiding ( read )

import Typing.Env
import Indices ( Ix(..), block )
import Types ( Type(..), RefType(..), Lft(..), shift )
import Syntax ( Deref(..), Term(..), Seq(..) )

import Data.Functor ( (<$) )
import Control.Monad.State.Class ( MonadState(..), gets, modify )
import Control.Monad.Reader.Class ( MonadReader(..), asks )
import Control.Monad.Reader ( runReader, Reader )
import Control.Monad ( when )
import Control.Monad.Except ( MonadError (throwError, catchError), runExceptT )

copy :: Type -> Bool
copy Unit = True
copy Bool = True
copy Int = True
copy (TRef _ Shr t) = copy t
copy _ = False

clone :: Type -> Bool
clone (TRef _ Uniq _) = False
clone _ = True

deref :: MonadReader Env m => Deref -> m Type
deref (Deref 0 ix) = do
    Bind _ status ty <- asks (`at` ix)
    case status of
        Moved -> undefined
        _ -> pure ty
deref p =
    deref (decr p) >>= \case
        TRef _ _ ty -> pure ty
        _ -> undefined
  where
    decr :: Deref -> Deref
    decr (Deref n ix) = Deref (n-1) ix

mut :: MonadReader Env m => Deref -> m Type
mut (Deref 0 ix) = do
    Bind mu status ty <- asks (`at` ix)
    case (mu, status) of
        (Mut, Own) -> pure ty
        (Mut, Moved) -> pure ty
        _ -> undefined
mut p = derefMut p

derefMut :: MonadReader Env m => Deref -> m Type
derefMut (Deref 0 ix) = do
    Bind _ status ty <- asks (`at` ix)
    case status of
        Own -> pure ty
        _ -> undefined
derefMut (Deref n ix) =
    derefMut (Deref (n-1) ix) >>= \case
        TRef _ Uniq ty -> pure ty
        _ -> undefined

-- TODO: move this
out :: MonadState s m => m a -> m (a, s)
out m = do
    state <- get
    val <- m
    pure (val, state)

class Typeable t where
    typeof :: MonadState Env m => t -> m Type

instance Typeable Term where
    typeof LitUnit = pure Unit
    typeof (LitInt n) = pure Int
    typeof LitTrue = pure Bool
    typeof LitFalse = pure Bool
    typeof (LitString str) = pure String
    typeof (Var ix) = do
        Bind _ status ty <- gets (`at` ix)
        let n = block ix
        if copy ty
            then case status of
                Own -> pure $ shift ty n
                Borrow Shr _ -> pure $ shift ty n
                _ -> undefined
            else case status of
                Own -> shift ty n <$ modify (poison ix)
                _ -> undefined
    typeof (Clone p) = gets . runReader $ deref p
    typeof (Assign p t) = do
        ty <- gets . runReader $ mut p
        ty' <- typeof t
        if ty == ty'
            then pure Unit
            else undefined
    typeof (Ref ix) = do
        Bind _ status ty <- gets (`at` ix)
        case status of
            Own -> TRef (Loc 0) Shr ty <$ modify (borrowShr ix)
            Borrow _ _ -> pure $ TRef (Loc 0) Shr ty
            _ -> undefined
    typeof (RefMut ix) = do
        Bind mu status ty <- gets (`at` ix)
        case (mu, status) of
            (Mut, Own) -> TRef (Loc 0) Uniq ty <$ modify (borrowUniq ix) 
            _ -> undefined
    typeof (IfThenElse cond t1 t2) = do
        tyCond <- typeof cond
        when (tyCond /= Bool) undefined
        env <- get
        (ty1, env') <- out $ typeof t1
        (ty2, env'') <- out $ typeof t2 <* put env
        if ty1 == ty2 && env' == env''
            then pure ty1
            else undefined
    typeof (Block seq) = do
        ty <- typeof seq <* modify ([] :)
        modify $ tail . endlft
        pure $ shift ty $ -1

instance Typeable Seq where
    typeof (Let _ t seq) = do
        ty <- typeof t
        modify $ insert Imm ty
        typeof seq
    typeof (LetMut _ t seq) = do
        ty <- typeof t
        modify $ insert Mut ty
        typeof seq
    typeof (Seq t seq) = typeof t *> typeof seq
    typeof (Final t) = typeof t
