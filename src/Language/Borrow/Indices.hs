module Language.Borrow.Indices 
    ( Ix(..)
    , toTuple
    , (<+>)
    )
  where

-- | A De Bruijn Index in a context with blocks.
data Ix = Ix
    { block  :: !Int    -- ^ The block-index
    , offset :: !Int    -- ^ The offset into the block
    } 
   deriving (Eq, Show)

-- | Transforms an index into a pair of integers.
toTuple :: Ix -> (Int, Int)
toTuple (Ix n k) = (n, k)

instance Semigroup Ix where
    (Ix 0 n) <> (Ix b' n') = Ix b' (n + n')
    (Ix b n) <> (Ix b' n') = Ix (b + b') n

instance Monoid Ix where
    mempty = Ix 0 0

-- | The shifting operation for indices:
--
-- If @ix@ is the index of a variable, and its contents is a
-- reference to @ix'@, then the index of the referenced value
-- in the current context is @ix' <+> ix@.
(<+>) :: Ix -> Ix -> Ix
ix <+> (Ix b n) = ix <> Ix b (n+1)