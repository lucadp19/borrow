module Typing.Check where

import Typing.Env
import Indices ( Ix(..) )
import Types ( Type(..), RefType(..), Lft(..), shift )
import Syntax ( Deref(..), Term(..), Seq(..) )

copy :: Type -> Bool
copy Unit = True
copy Bool = True
copy Int = True
copy (TRef _ Shr t) = copy t
copy _ = False

clone :: Type -> Bool
clone (TRef _ Uniq _) = False
clone _ = True

deref :: Env -> Deref -> Type
deref env (Deref 0 ix) =
    let Bind _ status ty = env `at` ix in
    case status of
        Moved -> undefined
        _ -> ty
deref env (Deref n ix) = 
    case deref env (Deref (n-1) ix) of
        TRef _ _ ty -> ty
        _ -> undefined

mut :: Env -> Deref -> Type
mut env (Deref 0 ix) = 
    let Bind m s ty = env `at` ix in
    case (m, s) of
        (Mut, Own) -> ty
        (Mut, Moved) -> ty
        _ -> undefined
mut env deref = derefMut env deref

derefMut :: Env -> Deref -> Type
derefMut env (Deref 0 ix) = 
    let Bind _ status ty = env `at` ix in
    case status of
        Own -> ty
        _ -> undefined
derefMut env (Deref n ix) =
    case derefMut env (Deref (n-1) ix) of
        TRef _ Uniq ty -> ty
        _ -> undefined

class Typeable t where
    typeof :: Env -> t -> (Type, Env)

instance Typeable Term where
    typeof env LitUnit = (Unit, env)
    typeof env (LitInt n) = (Int, env)
    typeof env LitTrue = (Bool, env)
    typeof env LitFalse = (Bool, env)
    typeof env (LitString str) = (String, env)
    typeof env (Var ix) = 
        let (Bind _ status ty') = env `at` ix in
        let (Ix n _) = ix in
        if copy ty'
            then case status of
                Own -> (shift ty' n, env)
                Borrow Shr _ -> (shift ty' n, env)
                _ -> undefined
            else case status of
                Own -> (shift ty' n, poison ix env)
                _ -> undefined
    typeof env (Clone p) = 
        let ty = deref env p in
            (ty, env)
    typeof env (Assign p t) =
        let ty = mut env p in
        let (ty', env') = typeof env t in
        if ty == ty' 
            then (Unit, env')
            else undefined
    typeof env (Ref ix) =
        let (Bind _ status ty) = env `at` ix in
        case status of
            Own -> (TRef (Loc 0) Shr ty, borrowShr ix env)
            Borrow _ _ -> (TRef (Loc 0) Shr ty, env)
            _ -> undefined
    typeof env (RefMut ix) =
        let (Bind mu status ty) = env `at` ix in
        case (mu, status) of
            (Mut, Own) -> (TRef (Loc 0) Uniq ty, borrowUniq ix env) 
            _ -> undefined
    typeof env (IfThenElse cond t1 t2) =
        let (Bool, env') = typeof env cond in
        let (ty1, env'') = typeof env' t1 in
        let (ty2, env''') = typeof env' t2 in
        if ty1 == ty2 && env'' == env'''
            then (ty1, env'')
            else undefined
    typeof env (Block seq) = 
        let (ty, env') = typeof ([] : env) seq in
        let env'' = endlft env' in
            (shift ty $ -1, tail env'')

instance Typeable Seq where
    typeof env (Let _ t seq) = 
        let (ty, env') = typeof env t in
        let env'' = insert Imm ty env' in
            typeof env'' seq
    typeof env (LetMut _ t seq) = 
        let (ty, env') = typeof env t in
        let env'' = insert Mut ty env' in
            typeof env'' seq
    typeof env (Seq t seq) =
        let (ty, env') = typeof env t in
            typeof env' seq
    typeof env (Final t) = typeof env t