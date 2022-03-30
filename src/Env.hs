{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import Data.List ( elemIndex )
import qualified Data.Text as T
import Control.Monad.Except ( MonadError(..) )
import Indices ( Ix(..) )

newtype Block a = Block [a]
  deriving
    ( Eq
    , Show
    , Functor
    , Applicative 
    , Monad
    , Semigroup
    , Monoid
    )

blockAt :: Block a -> Int -> Maybe a
blockAt (Block b) = go b
  where
    go :: [a] -> Int -> Maybe a
    go [] _ = Nothing
    go (x:xs) 0 = pure x
    go (x:xs) k = go xs $ k-1

adjustBlock :: forall a. (a -> a) -> Int -> Block a -> Maybe (Block a)
adjustBlock f n (Block b) = Block <$> go b n
  where
    go :: [a] -> Int -> Maybe [a]
    go [] _ = Nothing
    go (x:xs) 0 = pure $ f x : xs
    go (x:xs) n = (x :) <$> go xs (n-1)

-- Environment type

newtype Env a = Env [Block a]
  deriving 
    ( Eq
    , Show
    , Functor
    , Semigroup
    , Monoid
    )
  
empty :: Env a
empty = Env []

at :: Env a -> Ix -> Maybe a
at (Env env) = go env
  where
    go :: [Block a] -> Ix -> Maybe a
    go [] _ = Nothing
    go (b:bs) (Ix 0 k) = b `blockAt` k
    go (b:bs) (Ix n k) = go bs $ Ix (n-1) k

pos :: forall a. (Eq a) => Env a -> a -> Maybe Ix
pos (Env env) val = go env 0
  where
    go :: [Block a] -> Int -> Maybe Ix
    go [] _ = Nothing
    go (Block b:bs) n = case val `elemIndex` b of
        Nothing -> go bs $ n+1
        Just k -> pure $ Ix n k

adjust :: forall a. (a -> a) -> Ix -> Env a -> Maybe (Env a)
adjust f ix (Env env) = Env <$> go env ix
  where
    go :: [Block a] -> Ix -> Maybe [Block a]
    go [] _ = Nothing
    go (b:bs) (Ix 0 k) = (: bs) <$> adjustBlock f k b
    go (b:bs) (Ix n k) = (b :) <$> go bs (Ix (n-1) k)

pushBlock :: Env a -> Env a
pushBlock (Env env) = Env $ mempty : env

pushBlockWithArgs :: [a] -> Env a -> Env a
pushBlockWithArgs args (Env env) = Env $ Block args : env

popBlock :: Env a -> Maybe (Env a)
popBlock (Env env) = case env of
    [] -> Nothing
    b:bs -> pure $ Env bs

insert :: a -> Env a -> Maybe (Env a)
insert val (Env env) = Env <$> case env of
    [] -> Nothing
    (Block l):bs -> pure $ Block (val:l) : bs