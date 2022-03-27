{-# LANGUAGE OverloadedStrings #-}
module Typing where

import Syntax
import Indices
import Types (Type(Int))

terms :: [Term]
terms = 
    [ term1
    , term2
    , term3
    , term4
    ]

term1 = Block $
    Let "x" (LitInt 4) $
    Let "y" (LitInt 5) $
    Final $ IfThenElse LitTrue (Var "y" $ Ix 0 0) (Var "x" $ Ix 0 1)

term2 = Block $
    LetMut "x" (LitInt 4) $
    Let "y" (LitInt 5) $
    Seq (Assign (Deref 0 (Ix 0 1)) (LitInt 7)) $
    Let "s" (LitString "string") $
    Final $ Var "s" $ Ix 0 0

term3 = Block $
    Let "x" (LitInt 5) $
    Let "p" (Ref "x" $ Ix 0 0) $
    Let "pp" (Ref "p" $ Ix 0 0) $
    Let "x1" (Clone $ Deref 2 $ Ix 0 0) $
    Final $ Var "x1" $ Ix 0 0

term4 = Block $
    Let "f" (Fn 0 ([Int, Int], Int) $ IfThenElse LitTrue (Var "x" $ Ix 0 0) (Var "y" $ Ix 0 0)) $
    LetMut "x" (LitInt 7) $
    Let "res" (Appl (Var "f" $ Ix 0 1) [] [LitInt 4, Var "x" $ Ix 0 0]) $
    Seq (Assign (Deref 0 $ Ix 0 1) (LitInt 0)) $
    Final LitTrue

termErr = Block $
    Let "x" (LitInt 7) $
    Let "p" (Ref "x" $ Ix 0 0) $
    Final $ Var "p" $ Ix 0 0 

termShift = Block $
    Let "x" (LitInt 7) $
    Final $ Block $
        Let "p" (Ref "x" $ Ix 1 0) $
        Let "y" LitUnit $
        Final $ Block $
            Let "pp" (Ref "p" $ Ix 1 1) $
            Let "y" LitUnit $
            Let "x1" (Clone $ Deref 2 $ Ix 0 1) $
            Final $ Var "x1" $ Ix 0 0
