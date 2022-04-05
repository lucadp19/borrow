{-# LANGUAGE OverloadedStrings #-}

module Language.Borrow.Parser.Parser ( parseTerm, parseBlock, parseType ) where

import Language.Borrow.Types ( Type(..), Lft(..), RefType(..) )
import Language.Borrow.Typing.TypeEnv ( MutStatus(..) )
import Language.Borrow.Syntax 
    ( Term(..)
    , Block(..)
    , Seq(..)
    , Deref(..)
    , BinaryOp(..)
    , UnaryOp(..) )
import Language.Borrow.Indices ( Ix(..) )

import qualified Language.Borrow.Parser.ParserEnv as PE

import Data.Void ( Void )
import qualified Data.Text as T

import Control.Applicative (optional)
import Control.Monad.Reader ( asks, MonadReader(local), Reader )
import Data.Maybe ( fromJust )

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
    ( (<|>)
    , optional
    , (<?>)
    , between
    , choice
    , many
    , manyTill
    , sepBy
    , MonadParsec(try, label)
    , ParsecT )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, space1 )
import Text.Megaparsec.Debug ( dbg )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, Prefix) )

-- | A type synonym for the Parser Monad.
type Parser = ParsecT Void T.Text (Reader PE.ParserEnv)

-- | A list of reserved keywords.
reserved :: [T.Text]
reserved = 
    [ "if", "then", "else", "True", "False", "Int", "Bool", "String", "fn"
    , "mut", "let", "clone" ]

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
stringLit = lexeme $ T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- | The parser for parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses the name of an identifier.
identifier :: Parser T.Text
identifier = lexeme $ label "identifier" $ do
    first <- letterChar <|> char '_'
    rest <- many $ alphaNumChar <|> char '_'
    check $ T.pack $ first : rest
  where
    check :: T.Text -> Parser T.Text
    check name
        | name `elem` reserved = fail $ "keyword `" <> T.unpack name <> "` cannot be an identifier."
        | otherwise = pure name

-- | Parses a variable, returning the variable's name and its index.
parseIx :: Parser (T.Text, Ix)
parseIx = do
    var <- identifier
    localVar <- asks $ PE.varPos var
    case localVar of
        Just ix  -> pure (var, ix)
        Nothing -> fail $ "No local variable named `" <> T.unpack var <> "` exists."

-- | Parses a variable and returns the correct index.
parseVar :: Parser Term
parseVar = parseIx >>= \(var, ix) -> pure $ Var var ix

-- | Parses a lifetime name and returns its value.
parseLft :: Parser Lft
parseLft = do
    _ <- symbol "'"
    lft <- identifier
    res <- asks $ PE.lftPos lft 
    case res of
        Just lftVal -> pure lftVal
        Nothing     -> fail $ "No lifetime named `" <> T.unpack lft <> "` exists." 

{----------- Type Parsers -----------}

-- | Parses a type.
parseType :: Parser Type
parseType = choice
    [ Bool   <$ symbol "Bool"
    , Int    <$ symbol "Int"
    , String <$ symbol "String"
    , Unit   <$ symbol "()"
    , parseRefTy
    , parseFnTy
    ]

-- | Parses a (shared or unique) reference.
parseRefTy :: Parser Type
parseRefTy = do
    _ <- symbol "&"
    lft <- parseLft
    isMut <- optional $ symbol "mut"
    case isMut of
        Just _ -> TRef lft Uniq <$> parseType
        Nothing -> TRef lft Shr <$> parseType

-- | Parses a function type.
parseFnTy :: Parser Type
parseFnTy = do
    _ <- symbol "for"
    lftList <- label "generic lifetimes in angular parenthesis" $ do
        _ <- symbol "<"
        lftList <- parseGenericLfts
        _ <- symbol ">"
        pure lftList
    paramTypes <- label "list of types in parenthesis" $ do
        _ <- symbol "("
        paramTypes <- local (const (PE.pushBlockWithArgs [] lftList PE.empty)) $ parseType `sepBy` symbol ","
        _ <- symbol ")"
        pure paramTypes
    outType <- label "'->' followed by output type" $ do
        _ <- symbol "->"
        (local $ const (PE.pushBlockWithArgs [] lftList PE.empty)) parseType
    pure $ TFn (length lftList) paramTypes outType
  where
    parseTypeMut :: Parser (MutStatus, Type)
    parseTypeMut = do
        isMut <- optional $ symbol "mut"
        ty <- parseType
        pure $ case isMut of
            Nothing -> (Imm, ty)
            Just _  -> (Mut, ty)
  
-- | Parses a list of generic lifetimes.
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

{---------- Block and sequences ---------}

-- | Parses a block.
parseBlock :: Parser Block
parseBlock = do
    lft <- (symbol "'" *> identifier) <?> "block lifetime identifier"
    _ <- symbol ":" *> symbol "{"
    seqn <- local (PE.pushBlock (Just lft)) parseSeqn
    _ <- symbol "}"
    pure $ Block seqn

-- | Parses a sequence of operations.
parseSeqn :: Parser Seq
parseSeqn = choice
    [ parseLet
    , parseSimpleSeq
    , parseFinal
    ]

-- | Parses a 'let' or a 'let mut'.
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

-- | Parses a sequence of instructions.
parseSimpleSeq :: Parser Seq
parseSimpleSeq = do
    term <- try $ do
        term <- parseTerm
        _ <- symbol ";"
        pure term
    Seq term <$> parseSeqn

-- | Parses the final term in a sequence.
parseFinal :: Parser Seq
parseFinal = Final <$> parseTerm

{---------- Term Parsers ---------}

-- | Parses a complex term.
parseComplex :: Parser Term
parseComplex = choice 
   [ parseAppl
   , parseIf
   , parseFn
   , parseAssign
   , TBlock <$> parseBlock
   , parseOp
   ]

-- | Parses a conditional expression.
parseIf :: Parser Term
parseIf = do
    _ <- symbol "if"
    cond <- parseTerm
    _ <- symbol "then"
    t1 <- parseTerm
    _ <- symbol "else"
    t2 <- parseTerm
    pure $ IfThenElse cond t1 t2

-- | Parses a simple term: a literal, a clone expression, a reference, a variable or a complex term in parenthesis.
parseSimple :: Parser Term
parseSimple = choice
    [ LitTrue <$ symbol "True"
    , LitFalse <$ symbol "False"
    , LitUnit <$ symbol "()"
    , LitInt <$> integer
    , LitString <$> stringLit
    , parseClone
    , parseRef
    , parsePrintln
    , parens parseTerm
    , parseVar
    ]

parseOp :: Parser Term
parseOp = makeExprParser parseSimple
    [ 
      [ Prefix (Unary Not <$ symbol "!")
      , Prefix (Unary Neg <$ symbol "-") 
      ]
    , [ InfixL (Binary And <$ symbol "&&")
      , InfixL (Binary Or <$ symbol "||")
      , InfixL (Binary Prod <$ symbol "*")
      , InfixL (Binary Div <$ symbol "/")
      , InfixL (Binary Concat <$ symbol "++")
      , InfixL (Binary Eq <$ symbol "==")
      , InfixL (Binary Less <$ symbol "<")
      , InfixL (Binary More <$ symbol ">")
      , InfixL (Binary Leq <$ symbol "<=")
      , InfixL (Binary Geq <$ symbol ">=")
      ]
    , [ InfixL (Binary Sum <$ symbol "+")
      , InfixL (Binary Sub <$ symbol "-")
      ]
    ]

-- | Parses a function application.
parseAppl :: Parser Term
parseAppl = do
    fn <- try $ do
        fn <- parseSimple <?> "a literal, a variable, a reference, a clone or a term in parenthesis"
        _ <- symbol "<"
        pure fn
    lftList <- (parseLft `sepBy` symbol ",") <?> "actual lifetime list in angular parenthesis"
    _ <- symbol ">"
    paramList <- label "list of actual parameters in parenthesis" $
        symbol "(" *> parseTerm `sepBy` symbol "," <* symbol ")"
    pure $ Appl fn lftList paramList

-- | Parses a function abstraction.
parseFn :: Parser Term
parseFn = do
    _ <- symbol "fn"
    lftList <- label "list of generic lifetimes in angular parenthesis" $ 
        symbol "<" *> parseGenericLfts <* symbol ">"
    params <- label "list of formal arguments in parenthesis" $
        symbol "(" *> 
            local (const $ PE.pushBlockWithArgs [] lftList PE.empty) (parseParam `sepBy` symbol ",") <*
            symbol ")"
    outTy <- label "'->' followed by output type" $ 
        symbol "->" *>
            local (const $ PE.pushBlockWithArgs [] lftList PE.empty) parseType
    _ <- symbol ":"
    lft <- (symbol "'" *> identifier) <?> "function body's lifetime identifier"
    symbol ":" *> symbol "{"
    seqn <- label "sequence of operations corresponding to function body" $
        local (const $ PE.pushBlockWithArgs (fst <$> params) ((lft, Loc 0) : lftList) PE.empty) parseSeqn
    symbol "}"
    pure $ Fn (length lftList) (snd <$> params, outTy) $ Block seqn
  where
    parseParam :: Parser (T.Text, (MutStatus, Type))
    parseParam = do
        isMut <- optional $ symbol "mut"
        var <- identifier
        _ <- symbol ":"
        ty <- parseType
        pure $ case isMut of
            Nothing -> (var, (Imm, ty))
            Just _  -> (var, (Mut, ty))

-- | Parses a dereference.
parseDeref :: Parser Deref
parseDeref = do
    derefs <- many $ char '*'
    (var, ix) <- parseIx
    pure $ Deref var (length derefs) ix

parsePrintln :: Parser Term
parsePrintln = do
    _ <- symbol "println"
    terms <- label "list of terms in parentheses" $
        symbol "(" *>
        parseTerm `sepBy` symbol "," <*
        symbol ")"
    pure $ Println terms

-- | Parses a clone expression
parseClone :: Parser Term
parseClone = do
    _ <- symbol "clone"
    Clone <$> parseDeref

-- | Parses an assignment
parseAssign :: Parser Term
parseAssign = do
    deref <- try $ parseDeref <* symbol ":="
    Assign deref <$> parseTerm

-- | Parses a (shared or unique) reference creation.
parseRef :: Parser Term
parseRef = do
    _ <- symbol "&"
    isMut <- optional $ symbol "mut"
    case isMut of
        Nothing -> Ref <$> parseDeref
        Just _ -> RefMut <$> parseDeref

-- | Parses a term.
parseTerm :: Parser Term
parseTerm = parseComplex
    