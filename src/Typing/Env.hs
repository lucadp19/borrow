{-# LANGUAGE PatternSynonyms #-}

module Typing.Env where

import Types ( Type, RefType )
import Indices (Ix(..))

type Env = [Block]

type Block = [Bind]

data Bind = Bind !MutStatus !BorrowStatus !Type

data MutStatus = Mut | Imm

data BorrowStatus = Own | Borrow !RefType !Int | Moved

poison :: Env -> Ix -> Env
poison ((bind : block) : env) (Ix 0 0) =
    let Bind mu _ ty = bind in
    let bind' = Bind mu Moved ty in
        (bind' : block) : env
poison ((bind : block) : env) (Ix 0 n) = 
    let (block' : env') = poison (block : env) (Ix 0 (n - 1)) in
        (bind : block') : env'
poison (block : env) (Ix b n) = block : poison env (Ix (b - 1) n)
poison _ _ = undefined
    
at :: Env -> Ix -> Bind
((bind : block) : env) `at` (Ix 0 0) = bind
((_ : block) : env) `at` (Ix 0 n) = (block : env) `at` Ix 0 (n-1)
(block : env) `at` (Ix b n) = env `at` Ix (b - 1) n
_ `at` _ = undefined

endlft :: Env -> Env
endlft = go 0
  where
    go :: Int -> Env -> Env
    go _ [] = []
    go n (block : env) = endBlockLft n block : env

    endBlockLft :: Int -> Block -> Block
    endBlockLft _ [] = []
    endBlockLft n (bind : block) =
        let Bind mu status ty = bind in
        let status' = case status of
                Borrow _ m | m == -n -> Own
                _ -> status
        in
            Bind mu status' ty : endBlockLft n block