{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Borrow.Env 
  ( Env
  , Block
  , empty
  , pos
  , at
  , adjust
  , insert
  , pushBlock
  , pushBlockWithArgs
  , popBlock
  )
where

import qualified Data.Text as T
import Data.List ( elemIndex )

import Language.Borrow.Indices ( Ix(..) )

-- | A block in an environment.
-- 
-- A block is simply a list of values.
type Block a = [a]

-- | Retrieves the value at the required position, if any.
blockAt :: Block a    -- ^ The given block.
        -> Int        -- ^ The given position.
        -> Maybe a
blockAt = go 
  where
    go :: [a] -> Int -> Maybe a
    go [] _ = Nothing
    go (x:xs) 0 = pure x
    go (x:xs) k = go xs $ k-1

-- | Adjust the value at a given position through a given function.
-- 
-- If there is no value at the given position, the function returns the initial block.
adjustBlock :: forall a. (a -> a) -- ^ The function to adjust the value.
            -> Int                -- ^ The position of the value to adjust.
            -> Block a            -- ^ The initial block.
            -> Block a
adjustBlock f n b = go b n
  where
    go :: [a] -> Int -> [a]
    go [] _ = []
    go (x:xs) 0 = f x : xs
    go (x:xs) n = x : go xs (n-1)

-- Environment type

-- | A generic environment.
--
-- An environment is a list of blocks.
type Env a = [Block a]

-- | Returns an empty environment.
empty :: Env a
empty = mempty

-- | Retrieves the value at the given position, if any.
at :: Env a     -- ^ The given environment.
   -> Ix        -- ^ The given position.
   -> Maybe a
at = go
  where
    go :: [Block a] -> Ix -> Maybe a
    go [] _ = Nothing
    go (b:bs) (Ix 0 k) = b `blockAt` k
    go (b:bs) (Ix n k) = go bs $ Ix (n-1) k

-- | Returns for the lowest index containing a given value, if any.
pos :: forall a. (Eq a) 
    => Env a      -- ^ The given environment.
    -> a          -- ^ The value to search.
    -> Maybe Ix
pos env val = go env 0
  where
    go :: Env a -> Int -> Maybe Ix
    go [] _ = Nothing
    go (b:bs) n = case val `elemIndex` b of
        Nothing -> go bs $ n+1
        Just k -> pure $ Ix n k

-- | Adjusts the value at a given position through a given function.
-- 
-- If there is no value at the given position, the function returns the initial environment.
adjust :: forall a. (a -> a)  -- ^ The function used to adjust the value.
       -> Ix                  -- ^ The index of the value.
       -> Env a               -- ^ The given environment.
       -> Env a
adjust f ix env = go env ix
  where
    go :: Env a -> Ix -> Env a
    go [] _ = []
    go (b:bs) (Ix 0 k) = adjustBlock f k b : bs
    go (b:bs) (Ix n k) = b : go bs (Ix (n-1) k)

-- | Adds an empty block at the top of the environment.
pushBlock :: Env a -> Env a
pushBlock = (mempty :)

-- | Adds a new block at the top of the environment, containing the given elements.
pushBlockWithArgs :: [a] -> Env a -> Env a
pushBlockWithArgs = (:)

-- | Removes the block on top of the environment. If the environment is empty, returns @Nothing@.
popBlock :: Env a -> Maybe (Env a)
popBlock env = case env of
    [] -> Nothing
    b:bs -> pure bs

-- | Inserts a new value into the environment. If the environment is empty, returns @Nothing@.
insert :: a -> Env a -> Maybe (Env a)
insert val env = case env of
    [] -> Nothing
    b:bs -> pure $ (val:b) : bs