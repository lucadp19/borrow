module Language.BorrowLang.Parser.ParserEnv where

import qualified Data.Text as T

import qualified Language.BorrowLang.Env as E
import Language.BorrowLang.Types ( Lft(..) )
import Language.BorrowLang.Indices ( Ix(..) )

data ParserEnv = ParserEnv
    { vars :: E.Env T.Text
    , lfts :: E.Env (T.Text, Lft)
    }

empty :: ParserEnv
empty = ParserEnv E.empty E.empty

insertVar :: T.Text -> ParserEnv -> Maybe ParserEnv
insertVar var env = E.insert var (vars env) >>= \vars' -> Just $ env { vars = vars' }

insertLft :: (T.Text, Lft) -> ParserEnv -> Maybe ParserEnv
insertLft lft env = E.insert lft (lfts env) >>= \lfts' -> Just $ env { lfts = lfts' }

varPos :: T.Text -> ParserEnv -> Maybe Ix
varPos var env = E.pos (vars env) var

lftPos :: T.Text -> ParserEnv -> Maybe Lft
lftPos var env = go 0 var (lfts env)
  where
    go :: Int -> T.Text -> E.Env (T.Text, Lft) -> Maybe Lft
    go n _ [] = Nothing
    go n var (b:bs) = case inner var b of
        Nothing           -> go (n+1) var bs
        Just (Loc m)      -> pure $ Loc $ n+m
        Just (LftVar m k) -> pure $ LftVar (n+m) k
    
    inner :: T.Text -> E.Block (T.Text, Lft) -> Maybe Lft
    inner _ [] = Nothing
    inner var ((v, lft):vs) = if var == v then pure lft else inner var vs

pushBlock :: Maybe T.Text -> ParserEnv -> ParserEnv
pushBlock (Just lftName) env = env { vars = E.pushBlock (vars env), lfts = E.pushBlockWithArgs [(lftName, Loc 0)] (lfts env)}
pushBlock Nothing env = env { vars = E.pushBlock (vars env), lfts = E.pushBlock (lfts env) }

pushBlockWithArgs :: [T.Text] -> [(T.Text, Lft)] -> ParserEnv -> ParserEnv
pushBlockWithArgs newVars newLfts env = env { vars = newVars : vars env, lfts = newLfts : lfts env }

popBlock :: ParserEnv -> Maybe ParserEnv
popBlock env = do
    vars' <- E.popBlock (vars env) 
    lfts' <- E.popBlock (lfts env)
    pure $ env { vars = vars', lfts = lfts' }