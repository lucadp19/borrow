module Language.BorrowLang.Interpreter.Store where

import qualified Language.BorrowLang.Env as E
import qualified Language.BorrowLang.Interpreter.Heap as H
import Language.BorrowLang.Interpreter.Values ( Value(..) )
import Language.BorrowLang.Indices (Ix)

data Store a = Store 
    { heap :: H.Heap a
    , tmp :: [[H.Location]]
    , env :: E.Env a
    }

empty :: Store a
empty = Store
    { heap = H.empty
    , tmp = []
    , env = E.empty
    }

-- Temporary stack operations

pushTmp :: H.Location -> Store a -> Maybe (Store a)
pushTmp l store = case tmp store of
    [] -> Nothing
    b:bs -> Just $ store { tmp = (l:b):bs }

popTmp :: Store a -> (Maybe H.Location, Store a)
popTmp store = case tmp store of
    (loc:b):bs -> (Just loc, store { tmp = b:bs })
    _ -> (Nothing, store)

-- Environment operations

insert :: a -> Store a -> Maybe (Store a)
insert val store = E.insert val (env store) >>= \env' -> Just $ store { env = env' }

at :: Store a -> Ix -> Maybe a
at store = E.at (env store)

adjust :: (a -> a) -> Ix -> Store a -> Store a
adjust f ix store = store { env = E.adjust f ix (env store) }

-- Heap operations

heapInsert :: a -> Store a -> Maybe (Store a)
heapInsert val store = 
    let (heap', l) = H.insert val (heap store)
        store' = store { heap = heap' }
    in pushTmp l store'

heapLookup :: H.Location -> Store a -> Maybe a
heapLookup loc store = H.lookup (heap store) loc
    
-- Block pushing and popping

pushBlock :: Store a -> Store a
pushBlock s = s { tmp = [] : tmp s, env = E.pushBlock (env s) }

pushBlockWithArgs :: [a] -> Store a -> Store a
pushBlockWithArgs args s = s { tmp = [] : tmp s, env = E.pushBlockWithArgs args (env s) }

popBlock :: ([H.Location] -> E.Block a -> H.Heap a -> H.Heap a) -> Store a -> Maybe (Store a)
popBlock f s = case (tmp s, env s) of
    (t:ts, b:bs) -> Just $ s { tmp = ts, env = bs, heap = f t b $ heap s }
    _ -> Nothing