module Typing.Env where

import Types ( Type(..), RefType(..) )
import Indices (Ix(..))

type Env = [Block]

type Block = [Bind]

data Bind = Bind !MutStatus !BorrowStatus !Type
  deriving (Eq)

data MutStatus = Mut | Imm
  deriving (Eq)

data BorrowStatus = Own | Borrow !RefType !Int | Moved
  deriving (Eq)

at :: Env -> Ix -> Bind
((bind : _) : env) `at` (Ix 0 0) = bind
((_ : block) : env) `at` (Ix 0 n) = (block : env) `at` Ix 0 (n-1)
(block : env) `at` (Ix b n) = env `at` Ix (b - 1) n
_ `at` _ = undefined

insert :: MutStatus -> Type -> Env -> Env
insert mu ty env =
    let block : env' = env in
    let bind = Bind mu Own ty in
      (bind : block) : env'

endlft :: Env -> Env
endlft = go 0
  where
    go :: Int -> Env -> Env
    go _ [] = []
    go n (block : env) = endBlockLft n block : go (n+1) env

    endBlockLft :: Int -> Block -> Block
    endBlockLft _ [] = []
    endBlockLft n (bind : block) =
        let Bind mu status ty = bind in
        let status' = case status of
                Borrow _ m | m == -n -> Own
                _ -> status
        in
            Bind mu status' ty : endBlockLft n block

updateIx :: (Bind -> Bind) -> Ix -> Env -> Env
updateIx f = go
  where
    go :: Ix -> Env -> Env
    go (Ix 0 0) ((bind : block) : env) = (f bind : block) : env
    go (Ix 0 n) ((bind : block) : env) = 
        let (block' : env') = go (Ix 0 (n-1)) (block : env) in
            (bind : block') : env'
    go (Ix b n) (block : env) = block : go (Ix (b-1) n) env
    go _ _ = undefined

poison :: Ix -> Env -> Env
poison = updateIx f
  where
    f :: Bind -> Bind
    f (Bind mu _ ty) = Bind mu Moved ty

borrow :: RefType -> Ix -> Env -> Env
borrow refTy ix = updateIx f ix
  where
    depth :: Int
    depth = let (Ix n _) = ix in depth

    f :: Bind -> Bind
    f (Bind mu _ ty) = Bind mu (Borrow refTy $ -depth) ty  

borrowShr :: Ix -> Env -> Env
borrowShr = borrow Shr

borrowUniq :: Ix -> Env -> Env
borrowUniq = borrow Uniq