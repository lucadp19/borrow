-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BorrowLang.Env where

import qualified Data.Text as T
import Data.List ( elemIndex )
import Control.Monad.Except ( MonadError(..) )

import Language.BorrowLang.Indices ( Ix(..) )

type Block a = [a]

blockAt :: Block a -> Int -> Maybe a
blockAt = go 
  where
    go :: [a] -> Int -> Maybe a
    go [] _ = Nothing
    go (x:xs) 0 = pure x
    go (x:xs) k = go xs $ k-1

adjustBlock :: forall a. (a -> a) -> Int -> Block a -> Block a
adjustBlock f n b = go b n
  where
    go :: [a] -> Int -> [a]
    go [] _ = []
    go (x:xs) 0 = f x : xs
    go (x:xs) n = x : go xs (n-1)

-- Environment type

type Env a = [Block a]

empty :: Env a
empty = mempty

at :: Env a -> Ix -> Maybe a
at = go
  where
    go :: [Block a] -> Ix -> Maybe a
    go [] _ = Nothing
    go (b:bs) (Ix 0 k) = b `blockAt` k
    go (b:bs) (Ix n k) = go bs $ Ix (n-1) k

pos :: forall a. (Eq a) => Env a -> a -> Maybe Ix
pos env val = go env 0
  where
    go :: Env a -> Int -> Maybe Ix
    go [] _ = Nothing
    go (b:bs) n = case val `elemIndex` b of
        Nothing -> go bs $ n+1
        Just k -> pure $ Ix n k

adjust :: forall a. (a -> a) -> Ix -> Env a -> Env a
adjust f ix env = go env ix
  where
    go :: Env a -> Ix -> Env a
    go [] _ = []
    go (b:bs) (Ix 0 k) = adjustBlock f k b : bs
    go (b:bs) (Ix n k) = b : go bs (Ix (n-1) k)

pushBlock :: Env a -> Env a
pushBlock = (mempty :)

pushBlockWithArgs :: [a] -> Env a -> Env a
pushBlockWithArgs = (:)

popBlock :: Env a -> Maybe (Env a)
popBlock env = case env of
    [] -> Nothing
    b:bs -> pure  bs

insert :: a -> Env a -> Maybe (Env a)
insert val env = case env of
    [] -> Nothing
    b:bs -> pure $ (val:b) : bs