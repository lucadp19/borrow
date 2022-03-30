module Indices where

data Ix = Ix !Int !Int
    deriving (Eq)

block :: Ix -> Int
block (Ix n _) = n

offset :: Ix -> Int
offset (Ix _ n) = n

toTuple :: Ix -> (Int, Int)
toTuple (Ix n k) = (n, k)

instance Semigroup Ix where
    (Ix 0 n) <> (Ix b' n') = Ix b' (n + n')
    (Ix b n) <> (Ix b' n') = Ix (b + b') n

instance Monoid Ix where
    mempty = Ix 0 0

(<+>) :: Ix -> Ix -> Ix
ix <+> (Ix b n) = ix <> Ix b (n+1)