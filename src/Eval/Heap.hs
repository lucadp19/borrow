module Eval.Heap 
    ( Location
    , Heap
    , empty
    , insert
    , delete
    , Eval.Heap.lookup
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M ( empty, insert, member, delete, lookup )

import qualified Data.Text as T

data Heap v = Heap
    { mem :: IntMap v
    , size :: !Int
    , free :: [Int]
    } 

newtype Location = Location Int

empty :: Heap v
empty = Heap 
    { mem = M.empty
    , size = 0
    , free = []
    }

insert :: Heap v -> v -> (Heap v, Location)
insert h v = case free h of
    -- free list is empty
    [] -> let l = Location (size h) in
        let h' = h { 
              mem = M.insert (size h) v (mem h)
            , size = (size h) + 1
            } in
        (h', l)
    -- free list has at least one element
    f:fs -> let l = Location f in
        let h' = h { 
              mem = M.insert f v (mem h)
            , size = (size h) + 1
            , free = fs
            } in
        (h', l)

delete :: Heap v -> Location -> Heap v
delete h (Location l)
    | M.member l (mem h) = h 
        { mem = M.delete l (mem h)
        , size = (size h) - 1
        , free = l : (free h)
        }
    | otherwise = h

lookup :: Heap v -> Location -> Maybe v
lookup h (Location l) = M.lookup l (mem h)