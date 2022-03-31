{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BorrowLang.Parser.Parser where

import qualified Language.BorrowLang.Parser.ParserEnv as PE
import Language.BorrowLang.Types ( Type(..), Lft(..), RefType(..) )
import Language.BorrowLang.Syntax ( Term(..), Block(..), Seq(..), Deref(..) )
import Language.BorrowLang.Indices ( Ix(..) )

import Data.Void ( Void )
import Data.List ( foldl1' )

import qualified Data.Text as T

import Control.Monad.Reader
import Data.Maybe ( fromJust )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug ( dbg )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Language.BorrowLang.App ( ParserApp )
import Control.Applicative (optional)

type Parser = ParsecT Void T.Text ParserApp

reserved :: [T.Text]
reserved = 
    [ "if", "then", "else", "True", "False", "Int", "Bool", "String", "fn"
    , "mut", "let" ]

-- | The whitespace lexer.
ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | The parser for lexemes.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- | The textual symbol parser.
symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

-- | The parser for integer literals.
integer :: Parser Int
integer = lexeme L.decimal

-- | The parser for string literals.
stringLit :: Parser T.Text
stringLit = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | The parser for parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses the name of an identifier.
identifier :: Parser T.Text
identifier = lexeme $ do
    ident <- some letterChar
    check $ T.pack ident
  where
    check :: T.Text -> Parser T.Text
    check name
        | name `elem` reserved = fail $ "keyword `" <> T.unpack name <> "` cannot be an identifier."
        | otherwise = pure name

parseIx :: Parser (T.Text, Ix)
parseIx = do
    var <- identifier
    localVar <- asks $ PE.varPos var
    case localVar of
        Just ix  -> pure $ (var, ix)
        Nothing -> fail $ "No local variable named " <> T.unpack var <> " exists."

-- | Parses a variable and returns the correct index.
parseVar :: Parser Term
parseVar = do
    var <- identifier
    localVar <- asks $ PE.varPos var
    case localVar of
        Just ix  -> pure $ Var var ix
        Nothing -> fail $ "No local variable named " <> T.unpack var <> " exists."

-- | Parses a lifetime name and returns its value.
parseLft :: Parser Lft
parseLft = do
    _ <- symbol "'"
    lft <- identifier
    res <- asks $ PE.lftPos lft 
    case res of
        Just lftVal -> pure lftVal
        Nothing     -> fail $ "No lifetime named " <> T.unpack lft <> " exists." 

parseType :: Parser Type
parseType = choice
    [ Bool   <$ symbol "Bool"
    , Int    <$ symbol "Int"
    , String <$ symbol "String"
    , Unit   <$ symbol "()"
    , try parseRefMutTy
    , parseRefTy
    , parseFnTy
    ]

parseRefMutTy :: Parser Type
parseRefMutTy = do
    _ <- symbol "&"
    lft <- parseLft
    _ <- symbol "mut"
    TRef lft Uniq <$> parseType

parseRefTy :: Parser Type
parseRefTy = do
    _ <- symbol "&"
    lft <- parseLft
    TRef lft Shr <$> parseType

parseFnTy :: Parser Type
parseFnTy = do
    _ <- symbol "for"
    _ <- symbol "<"
    lftList <- parseGenericLfts
    _ <- symbol ">"
    _ <- symbol "("
    paramTypes <- local (const (PE.pushBlockWithArgs [] lftList PE.empty)) $ parseType `sepBy` symbol ","
    _ <- symbol ")"
    _ <- symbol "->"
    outType <- (local $ const (PE.pushBlockWithArgs [] lftList PE.empty)) parseType
    pure $ TFn (length lftList) paramTypes outType
  
parseGenericLfts :: Parser [(T.Text, Lft)]
parseGenericLfts = do
    lft <- optional $ parseGenericLft 0
    case lft of
        Nothing -> pure []
        Just lftVal -> (lftVal :) <$> go 1
  where
    go :: Int -> Parser [(T.Text, Lft)]
    go n = do
        lft <- optional $ symbol "," >> parseGenericLft n
        case lft of 
            Nothing -> pure []
            Just lftVal -> (lftVal :) <$> go (n+1)

    parseGenericLft :: Int -> Parser (T.Text, Lft)
    parseGenericLft n = do
        _ <- char '\''
        lftName <- identifier
        pure (lftName, LftVar 0 n)

parseBlock :: Parser Block
parseBlock = do
    lft <- symbol "'" *> identifier
    _ <- symbol ":" *> symbol "{"
    seqn <- local (PE.pushBlock (Just lft)) parseSeqn
    _ <- symbol "}"
    pure $ Block seqn

parseSeqn :: Parser Seq
parseSeqn = choice
    [ parseLet
    , try parseSimpleSeq
    , parseFinal
    ]

parseLet :: Parser Seq
parseLet = do
    _ <- symbol "let"
    isMut <- optional $ symbol "mut"
    var <- identifier
    _ <- symbol "="
    term <- parseTerm
    _ <- symbol ";"
    case isMut of
        Nothing -> Let var term <$> local (fromJust . PE.insertVar var) parseSeqn
        Just _  -> LetMut var term <$> local (fromJust . PE.insertVar var) parseSeqn

parseSimpleSeq :: Parser Seq
parseSimpleSeq = do
    term <- parseTerm
    _ <- symbol ";"
    Seq term <$> parseSeqn

parseFinal :: Parser Seq
parseFinal = Final <$> parseTerm

parseComplex :: Parser Term
parseComplex = choice 
   [ try parseAppl
   , parseIf
   , parseFn
   , try parseAssign
   , TBlock <$> parseBlock
   , parseSimple
   ]

parseIf :: Parser Term
parseIf = do
    _ <- symbol "if"
    cond <- parseTerm
    _ <- symbol "then"
    t1 <- parseTerm
    _ <- symbol "else"
    t2 <- parseTerm
    pure $ IfThenElse cond t1 t2

parseSimple :: Parser Term
parseSimple = choice
    [ LitTrue <$ symbol "True"
    , LitFalse <$ symbol "False"
    , LitUnit <$ symbol "()"
    , LitInt <$> integer
    , LitString <$> stringLit
    , parseVar
    , parseClone
    , parseRef
    , parens parseTerm
    ]

parseAppl :: Parser Term
parseAppl = do
    fn <- parseSimple
    _ <- symbol "<"
    lftList <- parseLft `sepBy` symbol ","
    _ <- symbol ">"
    _ <- symbol "("
    paramList <- parseTerm `sepBy` symbol ","
    _ <- symbol ")"
    pure $ Appl fn lftList paramList

parseFn :: Parser Term
parseFn = do
    _ <- symbol "fn"
    _ <- symbol "<"
    lftList <- parseGenericLfts
    _ <- symbol ">"
    _ <- symbol "("
    params <- local (const $ PE.pushBlockWithArgs [] lftList PE.empty) $ parseParam `sepBy` symbol ","
    _ <- symbol ")" *> symbol "->"
    outTy <- local (const $ PE.pushBlockWithArgs [] lftList PE.empty) parseType
    _ <- symbol ":"
    lft <- symbol "'" *> identifier
    symbol ":" *> symbol "{"
    seqn <- local (const $ PE.pushBlockWithArgs (fst <$> params) ((lft, Loc 0) : lftList) PE.empty) parseSeqn
    symbol "}"
    pure $ Fn (length lftList) (snd <$> params, outTy) $ Block seqn
  where
    parseParam :: Parser (T.Text, Type)
    parseParam = do
        var <- identifier
        _ <- symbol ":"
        ty <- parseType
        pure (var, ty)

parseDeref :: Parser Deref
parseDeref = do
    derefs <- many $ char '*'
    (_, ix) <- parseIx
    pure $ Deref (length derefs) ix

parseClone :: Parser Term
parseClone = do
    _ <- symbol "clone"
    Clone <$> parseDeref

parseAssign :: Parser Term
parseAssign = do
    deref <- parseDeref
    _ <- symbol ":="
    Assign deref <$> parseTerm

parseRef :: Parser Term
parseRef = do
    _ <- symbol "&"
    isMut <- optional $ symbol "mut"
    case isMut of
        Nothing -> Ref "" <$> parseDeref
        Just _ -> RefMut "" <$> parseDeref
    

parseTerm :: Parser Term
parseTerm = parseComplex
    