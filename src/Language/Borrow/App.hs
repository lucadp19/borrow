module Language.Borrow.App ( run ) where

import Language.Borrow.Interpreter.Store ( Store )
import Language.Borrow.Interpreter.Values ( Value )
import Language.Borrow.Typing.TypeEnv ( TEnv )
import Language.Borrow.Parser.ParserEnv ( ParserEnv )
import Language.Borrow.Parser.Parser ( parseTerm, parseBlock )

import Language.Borrow.Syntax ( Term )
import Language.Borrow.Types ( Type(..) )

import qualified Language.Borrow.Parser.ParserEnv as PE
import qualified Language.Borrow.Env as E
import qualified Language.Borrow.Typing.TypeEnv as TE
import qualified Language.Borrow.Interpreter.Store as S
import qualified Language.Borrow.Interpreter.Heap as H

import Language.Borrow.Typing.Check ( Typeable(typeof) )
import Language.Borrow.Interpreter.Eval ( Evaluable(eval) )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void ( Void )
import Control.Applicative ( Alternative((<|>)), optional )
import Control.Applicative.Combinators ( choice )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Except ( ExceptT, MonadError, runExceptT )
import Control.Monad.State.Strict ( StateT (runStateT) )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.Reader ( Reader, runReader )

import Options.Applicative
    ( Parser, strOption, option, helper
    , long, short, help, metavar
    , auto, flag', execParser, fullDesc, info, progDesc, header
    )

import Text.Megaparsec ( runParserT, errorBundlePretty, ParseErrorBundle, ParsecT )
import Text.Pretty.Simple 
    ( pPrintOpt
    , CheckColorTty(CheckColorTty)
    , OutputOptions(..)
    , defaultOutputOptionsDarkBg 
    )

-- | The possible command line options.
data AppOptions = AppOptions
    { file :: Maybe FilePath    -- ^ An optional path to a file with the program 
    , action :: Action          -- ^ The action to perform on the given program
    }

-- | The possible actions to perform on a program.
data Action 
    = Parse -- ^ Parses the program and shows the AST
    | Type  -- ^ Types the program
    | Exec  -- ^ Executes the program
  deriving (Read)

-- | Parses the command line options.
options :: Parser AppOptions
options = AppOptions <$> parseFile <*> parseAction

-- | Parses the filepath option, if present.
parseFile :: Parser (Maybe FilePath)
parseFile = optional $ strOption (long "file" <> short 'f' <> metavar "FILE" <> help "read the input from FILE") 

-- | Parses an action, if present.
parseAction :: Parser Action
parseAction = choice
    [ flag' Parse (long "parse" <> short 'p' <> help "parse and then show the parsed AST")
    , flag' Type (long "type" <> short 't' <> help "parse, type-check and then show the resulting type")
    , flag' Exec (long "exec" <> short 'e' <> help "parse, type-check and execute the program")
    , pure Exec
    ]

-- | Runs the Borrow Interpreter.
run :: IO ()
run = do
    opts <- execParser $ info 
        (helper <*> options) 
        (fullDesc <> header "the interpreter for the Borrow language")
    program <- case file opts of
        Just filePath -> TIO.readFile filePath
        Nothing -> TIO.getContents 
    case action opts of
        Parse -> case parse program parseTerm of
            Left err -> putStrLn $ errorBundlePretty err
            Right term -> pPrintOpt CheckColorTty printOpts term
        Type -> typecheck program
        Exec -> exec program

printOpts :: OutputOptions
printOpts = defaultOutputOptionsDarkBg 
    { outputOptionsIndentAmount = 2
    , outputOptionsCompact = True
    , outputOptionsCompactParens = True
    }

-- | Applies the given parser on a string.
parse :: T.Text                                     -- ^ The string to parse
      -> ParsecT Void T.Text (Reader ParserEnv) t   -- ^ The parser
      -> Either (ParseErrorBundle T.Text Void) t
parse program parser = flip runReader PE.empty $ runParserT parser "" program
  
-- | Parses and then typechecks a term.
typecheck :: T.Text -> IO ()
typecheck program = case parse program parseTerm of
    Left parseErr -> putStrLn $ errorBundlePretty parseErr
    Right term -> do
        result <- runExceptT $ runStateT (typeof term) E.empty 
        case result of
            Left err -> putStrLn $ "❯ \x1b[31mtype error: \x1b[0m" <> T.unpack err
            Right (ty, _) -> putStrLn $ "❯ The given term has type `" <> show ty <> "`."

-- | Parses, types and then executes a program.
exec :: T.Text -> IO ()
exec program = case parse program parseBlock of
    Left parseErr -> putStrLn $ errorBundlePretty parseErr
    Right block -> do
        tyRes <- runExceptT $ runStateT (typeof block) E.empty 
        case tyRes of
            Left typeErr -> putStrLn $ "❯ \x1b[31mtype error: \x1b[0m" <> T.unpack typeErr
            Right (Unit, store) -> do
                execRes <- runExceptT $ runStateT (eval block) S.empty 
                case execRes of
                    Left err -> putStrLn $ "❯ \x1b[31mruntime error: \x1b[0m" <> T.unpack err
                    Right (val, store) -> pure () -- checkHeap store
            Right ty -> putStrLn "❯ \x1b[31merror:\x1b[0m a valid program is a block of type `Unit`"
  where
    checkHeap :: S.Store Value -> IO ()
    checkHeap store = if H.isEmpty (S.heap store) 
        then putStrLn "❯ The heap has been correctly cleaned."
        else do
            putStrLn "❯The heap has not been correctly cleaned. Printing the remaining heap."
            pPrintOpt CheckColorTty printOpts $ S.heap store