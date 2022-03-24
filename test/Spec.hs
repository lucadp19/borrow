{-# LANGUAGE FlexibleContexts #-}

import Typing.Check (typeof)
import Typing.Env (Env)
import Typing ( terms, termErr )
import Control.Monad ( (<=<) ) 
import Control.Monad.State.Lazy ( runStateT, StateT, join )
import Control.Monad.Except (ExceptT, runExceptT)
import Types ( Type )
import Syntax ( Term )

import Data.Text as T
import Data.Bifunctor (second)

main :: IO ()
main = mapM_ pet terms *> pet termErr
  where
    pet :: Term -> IO ()
    pet = print <=< eval . typeof

eval :: StateT Env (ExceptT T.Text IO) Type -> IO (Either T.Text (Type, Env))
eval = runExceptT . flip runStateT [[]]