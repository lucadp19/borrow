module Language.Borrow.Interpreter.Heap 
    ( Location
    , Heap
    , empty
    , insert
    , delete
    , deleteMultiple
    , lookup
    , adjust
    ) where

import Prelude hiding ( lookup )

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M 
    ( empty
    , insert
    , member
    , delete
    , lookup
    , adjust
    )

import qualified Data.Text as T

-- | A heap of values of a given type.
--
-- It supports insertions, lookup of values, removals and updates.
data Heap v = Heap
    { mem :: IntMap v -- ^ The inner heap, corresponding to an IntMap of the given values.
    , size :: !Int    -- ^ The number of elements currently stored.
    , free :: [Int]   -- ^ A free list, containing the indices of the free blocks.
    } 

-- | A location in a heap.
newtype Location = Location Int
  deriving (Eq)

-- | The empty heap.
empty :: Heap v
empty = Heap 
    { mem = M.empty
    , size = 0
    , free = []
    }

-- | Inserts a value into a heap, returning the modified heap and the location of the newly inserted value.
insert :: v                     -- ^ The value to be inserted.
       -> Heap v                -- ^ The given heap.
       -> (Heap v, Location)
insert v h = case free h of
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

-- | Deletes the value at a location from the heap. If the location is free, no changes are made.
delete :: Location  -- ^ The location to delete.
       -> Heap v    -- ^ The given heap.
       -> Heap v
delete (Location l) h
    | M.member l (mem h) = h 
        { mem = M.delete l (mem h)
        , size = (size h) - 1
        , free = l : (free h)
        }
    | otherwise = h

-- | Deletes all the given locations from the heap, ignoring free locations.
deleteMultiple :: [Location]    -- ^ The locations to delete.
               -> Heap v        -- ^ The given heap.
               -> Heap v
deleteMultiple [] h = h
deleteMultiple (l:ls) h = deleteMultiple ls $ delete l h

-- | Gets the value at a given position on the heap, if any.
lookup :: Heap v        -- ^ The given heap.
       -> Location      -- ^ The given position.
       -> Maybe v
lookup h (Location l) = M.lookup l (mem h)

-- | Adjusts the value at the given location through the given function.
-- If no value is present at the given location, the original heap is returned.
adjust :: (v -> v) -> Location -> Heap v -> Heap v
adjust f (Location l) h = h { mem = M.adjust f l (mem h) }