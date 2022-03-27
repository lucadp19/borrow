{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase #-}

module Main where

import Typing.Check (typeof)
import Typing.BetterEnv (TEnv)
import Control.Monad ( (<=<) ) 
import Control.Monad.State.Lazy ( runStateT, StateT, join )
import Control.Monad.Except (ExceptT, runExceptT)
import Types (Type(..))
import Syntax
import Indices ( Ix(..) )

import qualified Eval.State as S
import Eval.Eval ( Value, eval, fullEval )

import Data.Text as T
import Data.Bifunctor (second)

main :: IO ()
main = do
    mapM_ prt (termShift : termShift2 : termAssign : termAssign2 : termAssign3 : terms) 
    mapM_ pre [termShift, termShift2, term1, term2, term3, termAssign, termAssign2, termAssign3 ]
  where
    prt :: Term -> IO ()
    prt = print <=< runCheck . typeof

    pre :: Term -> IO ()
    pre = print <=< runEval . fullEval

runCheck :: StateT TEnv (ExceptT T.Text IO) Type -> IO (Either T.Text (Type, TEnv))
runCheck = runExceptT . flip runStateT mempty

runEval :: StateT (S.State Value) (ExceptT T.Text IO) Value -> IO (Either T.Text Value)
runEval s = runExceptT (runStateT s S.empty) >>= \case
    Left err -> pure $ Left err
    Right (ty, env) -> pure $ Right ty

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
    Final $ Var "x" $ Ix 0 2

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

termShift2 = Block $
    Let "x" (LitInt 15) $
    Final $ Block $
        Let "p" (Ref "x" $ Ix 1 0) $
        Let "y" LitUnit $
        Final $ Block $
            Let "pp" (Ref "p" $ Ix 1 1) $
            Let "y" LitUnit $
            Let "p1" (Clone $ Deref 1 $ Ix 0 1) $
            Let "y" LitUnit $
            Let "y" LitUnit $
            Let "x1" (Clone $ Deref 1 $ Ix 0 2) $
            Final $ Var "y" $ Ix 0 0

termAssign :: Term
termAssign = Block $
    LetMut "x" (LitInt 15) $
    Final $ Block $
        LetMut "p" (RefMut "x" $ Ix 1 0) $
        LetMut "y" LitUnit $
        Final $ Block $
            LetMut "pp" (RefMut "p" $ Ix 1 1) $
            LetMut "y" LitUnit $
            LetMut "y" LitUnit $
            Seq (Assign (Deref 2 $ Ix 0 2) $ LitInt 123) $
            Let "x_copy" (Clone (Deref 2 $ Ix 0 2)) $
            Final $ Var "x_copy" $ Ix 0 0

termAssign2 :: Term
termAssign2 = Block $
    LetMut "x" (LitInt 15) $
    LetMut "y" (LitInt 140) $
    Final $ Block $
        LetMut "p" (RefMut "x" $ Ix 1 1) $
        LetMut "tmp" LitUnit $
        Final $ Block $
            LetMut "pp" (RefMut "p" $ Ix 1 1) $
            LetMut "tmp" LitUnit $
            Seq (Assign (Deref 1 $ Ix 0 1) (RefMut "y" $ Ix 2 0)) $
            LetMut "tmp" LitUnit $
            Seq (Assign (Deref 2 $ Ix 0 2) $ LitInt 123) $
            Let "y_copy" (Clone (Deref 2 $ Ix 0 2)) $
            Final $ Var "y_copy" $ Ix 0 0

termAssign3 :: Term
termAssign3 = Block $
    LetMut "x" (LitInt 15) $
    Let "" LitUnit $
    LetMut "y" (LitInt 140) $
    Let "" LitUnit $
    LetMut "p" (RefMut "x" $ Ix 0 3) $
    Let "" LitUnit $
    Let "pp" (RefMut "p" $ Ix 0 1) $
    Seq (Assign (Deref 1 $ Ix 0 0) $ RefMut "y" $ Ix 0 4) $
    Seq (Assign (Deref 2 $ Ix 0 0) $ LitInt 12) $
    Let "y-copy" (Clone $ Deref 2 $ Ix 0 0) $
    Final $ Var "y-copy" $ Ix 0 0