{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.BorrowLang.App where

import Language.BorrowLang.Interpreter.Store ( Store )
import Language.BorrowLang.Interpreter.Values ( Value )
import Language.BorrowLang.Typing.TypeEnv (TEnv)
import Language.BorrowLang.Parser.ParserEnv ( ParserEnv )

import qualified Data.Text as T

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Except ( ExceptT, MonadError )
import Control.Monad.State.Strict ( StateT )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Reader (ReaderT, MonadReader)

newtype EvalApp a = EvalApp { runEApp :: StateT (Store Value) (ExceptT T.Text IO) a }
  deriving
    ( Functor 
    , Applicative
    , Monad
    , MonadState (Store Value)
    , MonadError T.Text
    , MonadIO
    )

newtype TypeApp a = TypeApp { runTApp :: StateT TEnv (ExceptT T.Text IO) a }
  deriving
    ( Functor 
    , Applicative
    , Monad
    , MonadState TEnv
    , MonadError T.Text
    , MonadIO
    )

newtype ParserApp a = ParserApp { runPApp :: ReaderT ParserEnv IO a }
  deriving
    ( Functor 
    , Applicative
    , Monad
    , MonadReader ParserEnv
    , MonadIO
    )
