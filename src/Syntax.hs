module Syntax where

import Data.Text as T
import Indices ( Ix(..) )
import Types ( Type, Lft )

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
    | Var T.Text Ix
    | Clone Deref
    | Assign Deref Term
    | Block Seq
    | Ref T.Text Ix
    | RefMut T.Text Ix
    | Fn Int ([Type], Type) Term
    | Appl Term [Lft] [Term]

data Deref
    = Deref !Int !Ix