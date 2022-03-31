module Language.BorrowLang.Syntax where

import Data.Text as T
import Language.BorrowLang.Indices ( Ix(..) )
import Language.BorrowLang.Types ( Type, Lft )

data Seq 
    = Let T.Text Term Seq
    | LetMut T.Text Term Seq
    | Seq Term Seq
    | Final Term
  deriving (Show)

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
    | TBlock Block
    | Ref T.Text Deref
    | RefMut T.Text Deref
    | Fn Int ([Type], Type) Block
    | Appl Term [Lft] [Term]
  deriving (Show)

newtype Block = Block Seq
  deriving (Show)

data Deref
    = Deref !Int !Ix
  deriving (Show)