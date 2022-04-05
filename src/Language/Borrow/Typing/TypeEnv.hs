{-# LANGUAGE FlexibleContexts #-}

module Language.Borrow.Typing.TypeEnv 
  ( TEnv
  , Bind(..)
  , MutStatus(..)
  , BorrowStatus(..)
  , insert
  , poison
  , borrow, borrowShr, borrowUniq
  , endlft
  )
where

import qualified Language.Borrow.Env as E

import Language.Borrow.Types ( RefType(..), Type )
import Language.Borrow.Indices ( Ix(..) )

-- | A type synonym for the Typing Environment.
type TEnv = E.Env Bind

-- | A binding in the Typing Environment.
data Bind = Bind
    { mutStatus :: !MutStatus
    , borStatus :: !BorrowStatus
    , bindType  :: !Type
    }
  deriving (Eq, Show)

-- | The mutability status of a binding: it can either be mutable or immutable.
data MutStatus = Mut | Imm
  deriving (Eq, Show)

-- | The borrow status of a variable.
data BorrowStatus 
  = Own     -- ^ The variable owns its contents, i.e. no borrows are active
  | Borrow  -- ^ The variable has been borrowed
      !RefType !Int 
  | Moved   -- ^ The variable's contents have been moved
  deriving (Eq, Show)

-- | Inserts a new binding into the type-environment.
insert :: MutStatus -> Type -> TEnv -> Maybe TEnv
insert mu ty = E.insert $ Bind mu Own ty

-- | Poisons the variable at the given index, i.e. moves its contents.
poison 
    :: Ix     -- ^ The given index
    -> TEnv   -- ^ The given type environment
    -> TEnv
poison = E.adjust f
  where
    f :: Bind -> Bind
    f bind = bind { borStatus = Moved }

-- | Borrows (mutably or immutably) the variable at the given index.
borrow 
    :: RefType  -- ^ Indicates whether the borrow is mutable or immutable
    -> Ix       -- ^ The given index
    -> TEnv     -- ^ The initial type environment
    -> TEnv
borrow refTy ix@(Ix b _) = E.adjust f ix
  where
    f :: Bind -> Bind
    f bind = bind { borStatus = status }

    status :: BorrowStatus
    status = Borrow refTy $ -b

-- | Immutably borrows the variable at the given index.
borrowShr :: Ix -> TEnv -> TEnv
borrowShr = borrow Shr

-- | Mutably borrows the variable at the given index.
borrowUniq :: Ix -> TEnv -> TEnv
borrowUniq = borrow Uniq

-- | Ends the current lifetime by removing all ending borrows.
endlft :: TEnv -> TEnv
endlft = go 0 
  where
    go :: Int -> [E.Block Bind] -> [E.Block Bind]
    go _ [] = []
    go n (b:bs) = (f n <$> b) : go (n+1) bs

    f :: Int -> Bind -> Bind
    f n bind = case borStatus bind of
        Borrow refTy k | k == -n -> bind { borStatus = Own }
        _ -> bind