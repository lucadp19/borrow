module Indices where

data Ix = Ix !Int !Int
    deriving (Eq)

instance Semigroup Ix where
    (Ix 0 n) <> (Ix b' n') = Ix b' (n + n')
    (Ix b n) <> (Ix b' n') = Ix (b + b') n

instance Monoid Ix where
    mempty = Ix 0 0

(<+>) :: Ix -> Ix -> Ix
ix <+> (Ix b n) = ix <+> Ix b (n+1)