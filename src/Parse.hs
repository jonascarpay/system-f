{-# LANGUAGE OverloadedStrings #-}

-- module Parse (Parser, Ident, exprParser) where
module Parse (Parser, Ident) where

import Control.Monad
import Data.List (foldl1')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = Parsec Void Text

-- type ExprParser = Parser (Expr () Ident)

type Ident = Text

-- exprParser :: ExprParser
-- exprParser = pSpace *> pExpr <* eof

-- pSpace :: Parser ()
-- pSpace = Lex.space space1 (Lex.skipLineComment "#") empty

-- symbol :: Text -> Parser ()
-- symbol = void . Lex.symbol pSpace

-- lexeme :: Parser a -> Parser a
-- lexeme = (<* pSpace)

-- pNum :: Parser Int
-- pNum = lexeme Lex.decimal

-- pAtom :: Parser Atom
-- pAtom = AInt <$> pNum

-- abstract1 :: (Eq a, Functor f) => a -> f a -> f (Maybe a)
-- abstract1 cap = fmap (\a -> if a == cap then Nothing else Just a)

-- pIdent :: Parser Ident
-- pIdent = lexeme $ do
--   h <- letterChar
--   t <- many alphaNumChar
--   pure $ T.pack (h : t)

-- parens :: Parser a -> Parser a
-- parens p = symbol "(" *> p <* symbol ")"

-- pLam :: ExprParser
-- pLam = do
--   symbol "λ"
--   args <- some pIdent
--   symbol "."
--   body <- pExpr
--   pure $ foldr (\arg -> Lam () . abstract1 arg) body args

-- pExpr :: ExprParser
-- pExpr = pLam <|> foldl1' (App ()) <$> some pArith

-- pArith :: ExprParser
-- pArith = do
--   t <- pTerm
--   ts <- many (symbol "+" *> pTerm)
--   pure $ foldr (Plus ()) t ts

-- pTerm :: ExprParser
-- pTerm =
--   choice
--     [ Atom () <$> pAtom <?> "atom",
--       Var () <$> pIdent <?> "identifier",
--       parens pExpr
--     ]
