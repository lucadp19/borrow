module Eval.State where

import qualified Eval.Heap as H
import qualified Env as E
import Indices (Ix)

data State a = State 
    { heap :: H.Heap a
    , tmp :: [[H.Location]]
    , env :: E.Env a
    }

empty :: State a
empty = State
    { heap = H.empty
    , tmp = []
    , env = E.empty
    }

pushTmp :: H.Location -> State a -> Maybe (State a)
pushTmp l state = case tmp state of
    [] -> Nothing
    b:bs -> Just $ state { tmp = (l:b):bs }

popTmp :: State a -> (Maybe H.Location, State a)
popTmp state = case tmp state of
    (l:b):bs -> (Just l, state { tmp = b:bs })
    _ -> (Nothing, state)

insert :: a -> State a -> Maybe (State a)
insert val state = E.insert val (env state) >>= \env' -> Just $ state { env = env' }

heapInsert :: a -> State a -> Maybe (State a)
heapInsert val state = 
    let (heap', l) = H.insert (heap state) val 
        state' = state { heap = heap' }
    in pushTmp l state'
    
at :: State a -> Ix -> Maybe a
at state = E.at (env state)

adjust :: (a -> a) -> Ix -> State a -> Maybe (State a)
adjust f ix state = E.adjust f ix (env state) >>= \env' -> Just $ state { env = env'}

pushBlock :: State a -> State a
pushBlock s = s { tmp = [] : tmp s, env = E.pushBlock (env s) }