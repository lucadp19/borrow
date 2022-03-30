{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase #-}

module Main where

import Typing.Check (typeof)
import Typing.BetterEnv (TEnv)
import Control.Monad ( (<=<) ) 
import Control.Monad.State.Lazy ( runStateT, StateT, join )
import Control.Monad.Except (ExceptT, runExceptT)
import Types
import Syntax
import Indices ( Ix(..) )

import qualified Eval.State as S
import Eval.Eval ( Value, eval, fullEval )

import Data.Text as T
import Data.Bifunctor (second)

main :: IO ()
main = do
    mapM_ (\t -> prt t *> pre t)
        [ term1
        , term2
        , term3
        , term4
        , termShift
        , termShift2
        , termAssign
        , termAssign2
        , termAssign3
        , termAssign4
        , termFn
        ]
  where
    prt :: Term -> IO ()
    prt term = runCheck (typeof term) >>= \case
        Left err -> print $ "typechecking error: " <> err
        Right ty -> print $ "typechecking: " <> show ty

    pre :: Term -> IO ()
    pre term = runEval (fullEval term) >>= \case
        Left err -> print $ "evaluation error: " <> err
        Right ty -> print $ "evaluation: " <> show ty

runCheck :: StateT TEnv (ExceptT T.Text IO) Type -> IO (Either T.Text Type)
runCheck s = runExceptT (runStateT s mempty) >>= \case
    Left err -> pure $ Left err
    Right (ty, env) -> pure $ Right ty

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

term1 = TBlock $ Block $
    Let "x" (LitInt 4) $
    Let "y" (LitInt 5) $
    Final $ IfThenElse LitTrue (Var "y" $ Ix 0 0) (Var "x" $ Ix 0 1)

term2 = TBlock $ Block $
    LetMut "x" (LitInt 4) $
    Let "y" (LitInt 5) $
    Seq (Assign (Deref 0 (Ix 0 1)) (LitInt 7)) $
    Let "s" (LitString "string") $
    Final $ Var "x" $ Ix 0 2

term3 = TBlock $ Block $
    Let "x" (LitInt 5) $
    Let "p" (Ref "x" $ Ix 0 0) $
    Let "pp" (Ref "p" $ Ix 0 0) $
    Let "x1" (Clone $ Deref 2 $ Ix 0 0) $
    Final $ Var "x1" $ Ix 0 0

term4 = TBlock $ Block $
    Let "f" (Fn 0 ([Int, Int], Int) $ Block $ Final $ IfThenElse LitTrue (Var "x" $ Ix 0 0) (Var "y" $ Ix 0 0)) $
    LetMut "x" (LitInt 7) $
    Let "res" (Appl (Var "f" $ Ix 0 1) [] [LitInt 4, Var "x" $ Ix 0 0]) $
    Seq (Assign (Deref 0 $ Ix 0 1) (LitInt 0)) $
    Final LitTrue

termErr = TBlock $ Block $
    Let "x" (LitInt 7) $
    Let "p" (Ref "x" $ Ix 0 0) $
    Final $ Var "p" $ Ix 0 0 

termShift = TBlock $ Block $
    Let "x" (LitInt 7) $
    Final $ TBlock $ Block $
        Let "p" (Ref "x" $ Ix 1 0) $
        Let "y" LitUnit $
        Final $ TBlock $ Block $
            Let "pp" (Ref "p" $ Ix 1 1) $
            Let "y" LitUnit $
            Let "x1" (Clone $ Deref 2 $ Ix 0 1) $
            Final $ Var "x1" $ Ix 0 0

termShift2 = TBlock $ Block $
    Let "x" (LitInt 15) $
    Final $ TBlock $ Block $
        Let "p" (Ref "x" $ Ix 1 0) $
        Let "y" LitUnit $
        Final $ TBlock $ Block $
            Let "pp" (Ref "p" $ Ix 1 1) $
            Let "y" LitUnit $
            Let "p1" (Clone $ Deref 1 $ Ix 0 1) $
            Let "y" LitUnit $
            Let "y" LitUnit $
            Let "x1" (Clone $ Deref 1 $ Ix 0 2) $
            Final $ Var "y" $ Ix 0 0

termAssign :: Term
termAssign = TBlock $ Block $
    LetMut "x" (LitInt 15) $
    Final $ TBlock $ Block $
        LetMut "p" (RefMut "x" $ Ix 1 0) $
        LetMut "y" LitUnit $
        Final $ TBlock $ Block $
            LetMut "pp" (RefMut "p" $ Ix 1 1) $
            LetMut "y" LitUnit $
            LetMut "y" LitUnit $
            Seq (Assign (Deref 2 $ Ix 0 2) $ LitInt 123) $
            Let "x_copy" (Clone (Deref 2 $ Ix 0 2)) $
            Final $ Var "x_copy" $ Ix 0 0

termAssign2 :: Term
termAssign2 = TBlock $ Block $
    LetMut "x" (LitInt 15) $
    LetMut "y" (LitInt 140) $
    Final $ TBlock $ Block $
        LetMut "p" (RefMut "x" $ Ix 1 1) $
        LetMut "tmp" LitUnit $
        Final $ TBlock $ Block $
            LetMut "pp" (RefMut "p" $ Ix 1 1) $
            LetMut "tmp" LitUnit $
            Seq (Assign (Deref 1 $ Ix 0 1) (RefMut "y" $ Ix 2 0)) $
            LetMut "tmp" LitUnit $
            Seq (Assign (Deref 2 $ Ix 0 2) $ LitInt 123) $
            Let "y_copy" (Clone (Deref 2 $ Ix 0 2)) $
            Final $ Var "y_copy" $ Ix 0 0

termAssign3 :: Term
termAssign3 = TBlock $ Block $
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

termAssign4 :: Term
termAssign4 = TBlock $ Block $
    Let "a" (LitInt 3) $
    Let "" LitUnit $
    LetMut "b" (LitInt 15) $
    Let "" LitUnit $
    LetMut "p" (Ref "a" $ Ix 0 3) $
    Let "" LitUnit $
    Seq (Assign (Deref 0 $ Ix 0 1) $ Ref "b" $ Ix 0 3) $
    Let "b-copy" (Clone $ Deref 1 $ Ix 0 1) $
    Final $ Var "b-copy" $ Ix 0 0

termFn :: Term
termFn = TBlock $ Block $
    Let "f" (Fn 1 ([TRef (LftVar 0 0) Shr Int, TRef (LftVar 0 0) Shr Int], Int) $ Block $
        Let "x1" (Clone $ Deref 1 $ Ix 0 1) $
        Final $ Var "x1" $ Ix 0 0) $
    Let "x" (LitInt 5) $
    Let "x-ref" (Ref "x" $ Ix 0 0) $
    Let "y" (LitInt 6) $
    Let "y-ref" (Ref "y" $ Ix 0 0) $
    Let "res" (Appl (Var "f" $ Ix 0 4) [Loc 0] [Var "x-ref" $ Ix 0 2, Var "y-ref" $ Ix 0 0]) $
    Final $ Var "res" $ Ix 0 0