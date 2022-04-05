{-# LANGUAGE OverloadedStrings #-}
module Language.Borrow.Syntax where

import qualified Data.Text as T ( Text )
import Language.Borrow.Indices ( Ix(..) )
import Language.Borrow.Types ( Type, Lft )
import Language.Borrow.Typing.TypeEnv ( MutStatus )

-- | A sequence of instructions in a block.
data Seq 
    = Let T.Text Term Seq       -- ^ A @let@ instruction, followed by other instructions
    | LetMut T.Text Term Seq    -- ^ A @let mut@ instruction, followed by other instructions
    | Seq Term Seq              -- ^ A term followed by other instructions
    | Final Term                -- ^ The final term in a sequence
  deriving (Eq, Show)

data Term
    = LitUnit             -- ^ A @()@ literal
    | LitTrue             -- ^ A @True@ literal
    | LitFalse            -- ^ A @False@ literal
    | LitInt Int          -- ^ An integer literal
    | LitString T.Text    -- ^ A string literal
    | IfThenElse          -- ^ A @if-then-else@ expression
        Term Term Term
    | Unary UnaryOp Term  -- ^ A unary operator
    | Binary              -- ^ A binary operator
        BinaryOp Term Term
    | Println [Term]      -- ^ A @println@ primitive
    | Var T.Text Ix       -- ^ A variable
    | Clone Deref         -- ^ A @clone@ instruction
    | Assign Deref Term   -- ^ An assignment
    | TBlock Block        -- ^ A block
    | Ref Deref           -- ^ A shared reference or reborrow
    | RefMut Deref        -- ^ A mutable reference or reborrow
    | Fn                  -- ^ A function abstraction
        Int 
        ([(MutStatus, Type)], Type) 
        Block
    | Appl                -- ^ A function application
        Term [Lft] [Term]
  deriving (Eq, Show)

-- | A block containing a sequence of instructions.
newtype Block = Block Seq
  deriving (Eq, Show)

-- | A dereference operation applied zero or more times.
data Deref
    = Deref !T.Text !Int !Ix
  deriving (Eq, Show)

data BinaryOp
    = Sum     -- ^ Integer sum.
    | Prod    -- ^ Integer multiplication.
    | Sub     -- ^ Integer subtraction.
    | Div     -- ^ Integer division.
    | Less    -- ^ Binary '<'.
    | More    -- ^ Binary '>'.
    | Eq      -- ^ Equality.
    | Leq     -- ^ Less or equal.
    | Geq     -- ^ Greater or equal.
    | And     -- ^ Boolean and.
    | Or      -- ^ Boolean or.
    | Concat  -- ^ String concatenation.
  deriving (Eq, Show)

data UnaryOp 
    = Neg   -- ^ Unary '-' for integers.
    | Not   -- ^ Unary boolean not.
  deriving (Eq, Show)

-- | Pretty-prints a binary operator.
prettyBin :: BinaryOp -> T.Text
prettyBin Sum   = "+"
prettyBin Prod  = "*"
prettyBin Sub   = "-"
prettyBin Div   = "/"
prettyBin Less  = "<"
prettyBin More  = ">"
prettyBin Eq    = "=="
prettyBin Leq   = "<="
prettyBin Geq   = ">="
prettyBin And   = "&&"
prettyBin Or    = "||"
prettyBin Concat = "++"

-- | Pretty-prints a unary operator.
prettyUnary :: UnaryOp -> T.Text
prettyUnary Neg = "-"
prettyUnary Not = "!"
