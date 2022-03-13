module Eval.Heap where

import Data.IntMap (IntMap)
import Data.IntMap.Strict as M

import Data.Text as T

data Heap v = Heap {
    mem :: IntMap v,
    free :: [Int]
} 

newtype Location = Location Int

insert :: Heap v -> v -> (Heap v, Location)
insert h v = case free h of
    [] -> let size = M.size $ mem h in
        (h { mem = M.insert size v (mem h) }, Location size)
    f:fs -> undefined -- TODO!
