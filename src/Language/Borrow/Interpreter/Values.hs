module Language.Borrow.Interpreter.Values where

import Language.Borrow.Indices ( Ix, toTuple )
import Language.Borrow.Interpreter.Heap ( Location )
import Language.Borrow.Syntax ( Seq )

import qualified Data.Text as T

-- | A value in the Borrow interpreter.
data Value
    = VUnit           -- ^ Unit value
    | VInt Int        -- ^ An integer
    | VBool Bool      -- ^ A boolean
    | VRef Ix         -- ^ A reference to a variable
    | VPtr Location   -- ^ A pointer to the heap
    | VString T.Text  -- ^ A string value
    | VCode Seq       -- ^ A function body
    | VMoved          -- ^ A moved value
  deriving (Eq)

instance Show Value where
    show VUnit          = "()"
    show (VInt n)       = show n
    show (VBool b)      = show b
    show (VRef ix)      = "ref " <> show (toTuple ix)
    show (VPtr _)       = "<heap ptr>"
    show (VString str)  = show str
    show (VCode _)      = "<code>"
    show VMoved         = "MOVED-VALUE" 
