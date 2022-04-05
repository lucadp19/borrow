{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module Language.Borrow.Typing.Check ( Typeable(..) ) where

import Prelude hiding ( read )

import qualified Language.Borrow.Env as E
import Language.Borrow.Typing.TypeEnv
    ( borrowShr, borrowUniq
    , endlft, insert, poison
    , Bind(..)
    , BorrowStatus(..)
    , MutStatus(..)
    , TEnv )

import Language.Borrow.Indices ( Ix(..), toTuple )
import Language.Borrow.Types ( Type(..), RefType(..), Lft(..), shift, (<:), prettyType, baseType )
import Language.Borrow.Syntax ( Deref(..), Term(..), Seq(..), Block(..), BinaryOp(..), UnaryOp(..), prettyBin )

import qualified Data.Text as T
import Data.Maybe ( fromJust )
import Data.String ( IsString(..) )
import Data.Functor ( ($>) )

import Control.Monad.State.Class ( MonadState(..), gets, modify )
import Control.Monad.Reader.Class ( MonadReader(..), asks )
import Control.Monad.Reader ( runReader, ReaderT (runReaderT) )
import Control.Monad ( when, unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( MonadError(throwError) )

-- | Returns true if and only if the type is copy.
copy :: Type -> Bool
copy Unit = True
copy Bool = True
copy Int = True
copy (TRef _ Shr t) = True
copy _ = False

-- | Returns true if and only if the type is clone.
clone :: Type -> Bool
clone (TRef _ Uniq _) = False
clone _ = True

-- | Immutably dereferences a @Deref@ - used in typing @clone@.
deref :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
deref (Deref name 0 ix) = do
    Bind _ status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Moved -> throwError $ "cannot dereference `" <> name <> "` as it has been moved"
        _ -> pure ty
deref (Deref name n ix) =
    deref (Deref name (n-1) ix) >>= \case
        TRef _ _ ty -> pure ty
        _ -> throwError "cannot dereference a non-pointer type"

-- | Used in typing the LHS in assignments.
mut :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
mut (Deref name 0 ix) = do
    Bind mu status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case (mu, status) of
        (Mut, Own) -> pure ty
        (Mut, Moved) -> pure ty
        (Mut, _) -> throwError $ "cannot assign to `" <> name <> "` since it is a borrowed variable"
        (Imm, _) -> throwError $ "cannot assign to `" <> name <> "` since it is an immutable variable"
mut p = derefMut p

-- | Mutably dereferences the LHS of an assignment.
derefMut :: (MonadReader TEnv m, MonadError T.Text m) => Deref -> m Type
derefMut (Deref name 0 ix) = do
    Bind _ status ty <- ask >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Own -> pure ty
        Borrow _ _ -> throwError $ "cannot mutably dereference `" <> name <> "` since it has been borrowed"
        Moved -> throwError $ "cannot mutably dereference `" <> name <> "` since it has been moved"
derefMut (Deref name n ix) =
    derefMut (Deref name (n-1) ix) >>= \case
        TRef _ Uniq ty -> pure ty
        TRef _ Shr _ -> throwError "cannot mutably dereference a shared reference"
        _ -> throwError "cannot mutably dereference a non-pointer type"

-- | Immmutably reborrows a value.
reborrow :: (MonadState TEnv m, MonadError T.Text m) => Deref -> m Type
reborrow (Deref name 0 ix) = do
    Bind _ status ty <- get >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Own -> modify (borrowShr ix) $> ty
        Borrow Shr _ -> pure ty
        Borrow Uniq _ -> throwError $ "cannot reborrow `" <> name <> "` since it has already been mutably borrowed"
        Moved -> throwError $ "cannot reborrow `" <> name <> "` since it has been moved"
reborrow (Deref name n ix) = 
    reborrow (Deref name (n-1) ix) >>= \case
        TRef _ _ ty -> pure ty
        _ -> throwError "cannot reborrow a non-pointer type"

-- | Mutably reborrows a value.
reborrowMut :: (MonadState TEnv m, MonadError T.Text m) => Deref -> m Type
reborrowMut (Deref name 0 ix) = do
    Bind _ status ty <- get >>= \env -> case env `E.at` ix of
        Just val -> pure val
        Nothing -> throwError $ invalidIxErr ix
    case status of
        Own -> modify (borrowUniq ix) $> ty
        Borrow Shr _ -> throwError $ "cannot mutably reborrow `" <> name <> "` since it has already been borrowed"
        Borrow Uniq _ -> throwError $ "cannot mutably reborrow `" <> name <> "` since it has already been mutably borrowed"
        Moved -> throwError $ "cannot mutably reborrow `" <> name <> "` since it has already been moved"
reborrowMut (Deref name n ix) = 
    reborrowMut (Deref name (n-1) ix) >>= \case
        TRef _ Uniq ty -> pure ty
        TRef _ Shr ty -> throwError "cannot mutably reborrow a shared reference"
        _ -> throwError "cannot mutably reborrow a non-pointer type"     

-- | A typeclass for all syntactic structures that are typeable in an environment.
class Typeable t where
    typeof 
        :: forall m. (MonadState TEnv m, MonadError T.Text m) 
        => t        -- ^ The structure to type
        -> m Type   -- ^ The resulting type

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
                Own -> pure . fromJust $ shift ty n -- n >= 0 implies that the result is a Just
                Borrow Shr _ -> pure . fromJust $ shift ty n -- n >= 0 implies that the result is a Just
                Borrow Uniq _ -> throwError $ "cannot use variable `" <> name <> "` as it is currently mutably borrowed"
                Moved -> throwError $ "cannot use variable `" <> name <> "` as it is has already been moved"
            else case status of
                Own -> do
                    modify $ poison ix
                    pure . fromJust $ shift ty n    -- n >= 0 implies that the result is a Just
                Borrow _ _ -> throwError $ "cannot use variable `" <> name <> "`as it is currently borrowed"
                Moved -> throwError $ "cannot use variable `" <> name <> "` as it has already been moved"
    -- Binary operations
    typeof (Binary op t1 t2) = case op of
        op | op `elem` [Sum, Sub, Prod, Div] -> do
            t1 <- typeof t1
            t2 <- typeof t2
            case (t1, t2) of
                (Int, Int) -> pure Int
                _ -> throwError $ "can only apply `" <> prettyBin op <> "` on values of type `Int`"
        op | op `elem` [Leq, Geq, Less, More] -> do
            t1 <- typeof t1
            t2 <- typeof t2
            case (t1, t2) of
                (Int, Int) -> pure Bool
                _ -> throwError $ "can only apply `" <> prettyBin op <> "` on values of type `Int`"
        op | op `elem` [And, Or] -> do
            t1 <- typeof t1
            t2 <- typeof t2
            case (t1, t2) of
                (Bool, Bool) -> pure Bool
                _ -> throwError $ "can only apply `" <> prettyBin op <> "` on values of type `Bool`"
        Concat -> do
            t1 <- typeof t1
            t2 <- typeof t2
            case (t1, t2) of
                (String, String) -> pure String
                _ -> throwError $ "can only apply `" <> prettyBin op <> "` on values of type `String`"
        Eq -> do
            t1 <- baseType <$> typeof t1
            t2 <- baseType <$> typeof t2
            if t1 /= t2 
                then throwError $ "can only apply `" <> prettyBin op <> "` on values of the same type"
                else pure Bool
    -- Unary operations
    typeof (Unary op t) = case op of
        Neg -> typeof t >>= \case
            Int -> pure Int
            _ -> throwError "can only apply unary `-` on values of type `Int`"
        Not -> typeof t >>= \case
            Bool -> pure Bool
            _ -> throwError "can only apply unary `!` on values of type `Bool`"
    -- Println
    typeof (Println terms) = typeAll terms $> Unit
      where
        typeAll :: [Term] -> m Type
        typeAll [] = pure Unit
        typeAll (t:ts) = typeof t *> typeAll ts
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
    typeof (Ref (Deref name 0 ix)) = do
        Bind _ status ty <- get >>= \env -> case env `E.at` ix of
            Just val -> pure val
            Nothing -> throwError $ invalidIxErr ix
        case status of
            Own -> TRef (Loc 0) Shr ty <$ modify (borrowShr ix)
            Borrow Shr _ -> pure $ TRef (Loc 0) Shr ty
            Borrow Uniq _ -> throwError $ "cannot create shared reference to `" <> name <> "` as it is currently mutably borrowed"
            Moved -> throwError $ "cannot create shared reference to `" <> name <> "` as it has been moved"
    -- Unique reference
    typeof (RefMut (Deref name 0 ix)) = do
        Bind mu status ty <- get >>= \env -> case env `E.at` ix of
            Just val -> pure val
            Nothing -> throwError $ invalidIxErr ix
        case (mu, status) of
            (Mut, Own) -> TRef (Loc 0) Uniq ty <$ modify (borrowShr ix)
            (Mut, Borrow _ _) -> throwError $ "cannot create mutable reference to `" <> name <> "` as it is already borrowed"
            (Mut, Moved) -> throwError $ "cannot create mutable reference to `" <> name <> "` as it has already been moved"
            (Imm, _) -> throwError $ "cannot create mutable reference to `" <> name <> "`as it has not been declared as `mut`" 
    -- Immutable reborrow
    typeof (Ref deref) = reborrow deref
    typeof (RefMut deref) = reborrowMut deref
    -- Conditional expression
    typeof (IfThenElse cond t1 t2) = do
        tyCond <- typeof cond
        when (tyCond /= Bool) $ throwError $ "type mismatch: expected `bool` but got `" <> prettyType tyCond <> "`"
        env <- get
        (ty1, env') <- (,) <$> typeof t1 <*> get
        (ty2, env'') <- do
            put env 
            (,) <$> typeof t1 <*> get
        if ty1 == ty2 && env' == env''
            then pure ty1
            else throwError "types and environment of if branches must be equal"
    -- Block
    typeof (TBlock block) = typeof block
    -- Function abstraction
    typeof (Fn n (params, outTy) (Block body)) = do
        wf n $ TFn n (snd <$> params) outTy -- checking that the type is valid
        env <- get
        let fnEnv = E.pushBlockWithArgs ((\(mu, ty) -> Bind mu Own ty) <$> params) mempty
        ty <- put fnEnv *> typeof body >>= \res -> case shift res (-1) of
            Nothing -> throwError "cannot return value from function as it does not live long enough"
            Just ty -> pure ty
        if ty <: outTy
            then TFn n (snd <$> params) outTy <$ put env
            else throwError $ "type mismatch: expected `" <> prettyType outTy <> "` but got `" <> prettyType ty <> "`"
      where
        wf :: Int -> Type -> m ()
        wf n Unit = pure ()
        wf n Int = pure ()
        wf n Bool = pure ()
        wf n String = pure ()
        wf n (TRef (LftVar 0 k) _ ty) 
            = if k < n then wf n ty 
                else throwError "function argument contains an unspecified generic lifetime"
        wf n (TRef {}) = throwError "function argument contains a non-generic lifetime"
        wf _ (TFn n' params' outTy') = mapM (wf n') params' *> wf n' outTy'
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

instance Typeable Block where
    typeof (Block seqn) = do
        ty <- modify E.pushBlock *> typeof seqn
        modify $ fromJust . E.popBlock . endlft
        case shift ty (-1) of
            Just ty' -> pure ty'
            Nothing -> throwError "cannot return value as it does not live long enough" 

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

invalidIxErr :: Ix -> T.Text
invalidIxErr ix = "invalid index: no value at position " <> (fromString . show . toTuple) ix