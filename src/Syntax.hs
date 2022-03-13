module Syntax where

import Data.Text as T
import Indices ( Ix(..) )

data Seq 
    = Let T.Text Term Seq
    | LetMut T.Text Term Seq
    | Seq Term Seq
    | Final Term

data Term
    = LitUnit
    | LitTrue
    | LitFalse
    | LitInt Int
    | LitString T.Text
    | IfThenElse Term Term Term
    | Var Ix
    | Clone Deref
    | Assign Deref Term
    | Block Seq
    | Ref Ix
    | RefMut Ix

data Deref
    = Deref !Int !Ix