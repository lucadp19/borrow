module Language.BorrowLang.Interpreter.Values where

import Language.BorrowLang.Indices ( Ix, toTuple )
import Language.BorrowLang.Interpreter.Heap ( Location )
import Language.BorrowLang.Syntax ( Seq )

import qualified Data.Text as T

data Value
    = VUnit
    | VInt Int
    | VBool Bool
    | VRef Ix
    | VPtr Location
    | VString T.Text
    | VCode Seq
    | VMoved

instance Show Value where
    show VUnit = "()"
    show (VInt n) = show n
    show (VBool b) = show b
    show (VRef ix) = "ref " <> show (toTuple ix)
    show (VPtr _) = "<heap ptr>"
    show (VString str) = show str
    show (VCode _) = "<code>"
    show VMoved = "MOVED" 

