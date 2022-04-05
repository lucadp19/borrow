module Language.Borrow.Interpreter.Store where

import qualified Language.Borrow.Env as E
import qualified Language.Borrow.Interpreter.Heap as H

import Language.Borrow.Interpreter.Values ( Value(..) )
import Language.Borrow.Indices ( Ix )

-- | The @Store@ in which a Borrow program is evaluated in.
--
-- A store is comprised of three parts: a heap, a stack of temporaries and an environment.
data Store a = Store 
    { heap :: H.Heap a
    , tmp :: [[H.Location]]
    , env :: E.Env a
    }

-- | The empty @Store@.
empty :: Store a
empty = Store
    { heap = H.empty
    , tmp = []
    , env = E.empty
    }

-- Temporary stack operations
-- | Pushes a heap location into the store.
pushTmp :: H.Location -> Store a -> Maybe (Store a)
pushTmp l store = case tmp store of
    [] -> Nothing
    b:bs -> Just $ store { tmp = (l:b):bs }

-- | Pops the heap location at the top of the store.
popTmp :: Store a -> (Maybe H.Location, Store a)
popTmp store = case tmp store of
    (loc:b):bs -> (Just loc, store { tmp = b:bs })
    _ -> (Nothing, store)

-- Environment operations
-- | Inserts a value inside the environment.
insert :: a -> Store a -> Maybe (Store a)
insert val store = E.insert val (env store) >>= \env' -> Just $ store { env = env' }

-- | Gets the value at a given index in the environment.
at :: Store a -> Ix -> Maybe a
at store = E.at (env store)

-- | Adjusts the value at a given index in the environment.
adjust :: (a -> a) -> Ix -> Store a -> Store a
adjust f ix store = store { env = E.adjust f ix (env store) }

-- Heap operations
-- | Inserts a new value into the heap and puts the newly generated pointer into the stack of temporary values.
heapInsert :: a -> Store a -> Maybe (Store a)
heapInsert val store = 
    let (heap', l) = H.insert val (heap store)
        store' = store { heap = heap' }
    in pushTmp l store'

-- | Lookup of a heap location.
heapLookup :: H.Location -> Store a -> Maybe a
heapLookup loc store = H.lookup (heap store) loc

-- | Adjusts the value of a heap location through the given function.
heapAdjust :: (a -> a) -> H.Location -> Store a -> Store a
heapAdjust f loc store = store { heap = H.adjust f loc (heap store) }
    
-- Block pushing and popping
-- | Adds a new block to the store.
pushBlock :: Store a -> Store a
pushBlock s = s { tmp = [] : tmp s, env = E.pushBlock (env s) }

-- | Adds a new block to the store, putting the given values into the environment.
pushBlockWithArgs :: [a] -> Store a -> Store a
pushBlockWithArgs args s = s { tmp = [] : tmp s, env = E.pushBlockWithArgs args (env s) }

-- | Pops a block from the environment.
-- 
-- Takes a helper function that extracts the pointers to remove from the top block in the environment.
popBlock 
    :: ([a] -> [H.Location])    -- ^ A function that extracts pointers from the top environment block
    -> Store a                  -- ^ The current store
    -> Maybe (Store a)
popBlock f s = case (tmp s, env s) of
    (t:ts, b:bs) -> Just $ s 
        { tmp = ts
        , env = bs
        , heap = H.deleteMultiple t $ H.deleteMultiple (f b) $ heap s 
        }
    _ -> Nothing