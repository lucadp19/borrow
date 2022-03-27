{-# LANGUAGE FlexibleContexts #-}

module Typing.BetterEnv 
  ( module Typing.BetterEnv
  , module E
  )
where

import qualified Env as E
import Types ( RefType(..), Type )
import Indices ( Ix, block )
import qualified Data.Text as T
import Control.Monad.Except ( MonadError(..) )

type TEnv = E.Env Bind

data Bind = Bind !MutStatus !BorrowStatus !Type
  deriving (Eq, Show)

data MutStatus = Mut | Imm
  deriving (Eq, Show)

data BorrowStatus = Own | Borrow !RefType !Int | Moved
  deriving (Eq, Show)

insert :: MutStatus -> Type -> TEnv -> Maybe TEnv
insert mu ty = E.insert $ Bind mu Own ty

poison :: Ix -> TEnv -> Maybe TEnv
poison = E.adjust f
  where
    f :: Bind -> Bind
    f (Bind mu _ ty) = Bind mu Moved ty

borrow :: RefType -> Ix -> TEnv -> Maybe TEnv
borrow refTy ix = E.adjust f ix
  where
    f :: Bind -> Bind
    f (Bind mu _ ty) = Bind mu status ty

    status :: BorrowStatus
    status = Borrow refTy $ -block ix

borrowShr :: Ix -> TEnv -> Maybe TEnv
borrowShr = borrow Shr

borrowUniq :: Ix -> TEnv -> Maybe TEnv
borrowUniq = borrow Uniq

endlft :: TEnv -> TEnv
endlft (E.Env env) = E.Env $ go 0 env
  where
    go :: Int -> [E.Block Bind] -> [E.Block Bind]
    go _ [] = []
    go n (E.Block b:bs) = E.Block (f n <$> b) : go n bs

    f :: Int -> Bind -> Bind
    f n bind = case bind of
        Bind mu (Borrow refTy k) ty | k == -n -> Bind mu Own ty
        b -> b