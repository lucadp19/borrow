module Typing.Check where

import Typing.Env
import Types ( Type(..), RefType(..) )
import Syntax ( Deref(..), Term(..) )

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

typeof :: Env -> Term -> (Type, Env)
typeof env LitUnit = (Unit, env)
typeof env (LitInt n) = (Int, env)
typeof env LitTrue = (Bool, env)
typeof env LitFalse = (Bool, env)
typeof env (LitString str) = (String, env)
typeof _ _ = undefined