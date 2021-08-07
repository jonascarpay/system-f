{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse (Parser, Ident, exprParser, typeParser) where

import Control.Monad
import Data.List (foldl', foldl1')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Expr
import Rebound
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = Parsec Void Text

type ExprParser = Parser (Expr Ident Ident)

type TypeParser = Parser (Type Ident)

type Ident = Text

exprParser :: ExprParser
exprParser = pSpace *> pExpr <* eof

typeParser :: TypeParser
typeParser = pSpace *> pType <* pSpace

pSpace :: Parser ()
pSpace = Lex.space space1 (Lex.skipLineComment "#") empty

symbol :: Text -> Parser ()
symbol = void . Lex.symbol pSpace

lexeme :: Parser a -> Parser a
lexeme = (<* pSpace)

-- pNum :: Parser Int
-- pNum = lexeme Lex.decimal

-- pAtom :: Parser Atom
-- pAtom = AInt <$> pNum

pUnit :: ExprParser
pUnit = Unit <$ symbol "()"

pIdent :: Parser Ident
pIdent = lexeme $ do
  h <- letterChar
  t <- many alphaNumChar
  pure $ T.pack (h : t)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

pTLam :: ExprParser
pTLam = do
  symbol "Λ"
  args <- some pIdent
  symbol "."
  body <- pExpr
  pure $ foldr (\arg -> TLam . abstract1Over exprTypes arg) body args

pType :: TypeParser
pType = pForall <|> pArr

pArr :: TypeParser
pArr = do
  h <- pTypeTerm
  t <- many (symbol "->" *> pTypeTerm)
  pure $ foldr1 TArr (h : t)

pTypeTerm :: TypeParser
pTypeTerm =
  choice
    [ TUnit <$ symbol "()",
      TVar <$> pIdent,
      parens pType
    ]

pForall :: TypeParser
pForall = do
  symbol "∀"
  args <- some pIdent
  symbol "."
  body <- pType
  pure $ foldr (\arg -> TForall . abstract1 arg) body args

pLam :: ExprParser
pLam = do
  symbol "λ"
  args <- some $
    parens $ do
      arg <- pIdent
      symbol ":"
      typ <- pType
      pure (arg, typ)
  symbol "."
  body <- pExpr
  pure $ foldr (\(arg, typ) -> Lam typ . abstract1 arg) body args

pApp :: ExprParser
pApp = do
  h <- pTerm
  t <-
    many $
      choice
        [ symbol "@" *> fmap (flip TApp) pTypeTerm,
          flip App <$> pTerm
        ]
  pure $ foldl' (\x f -> f x) h t

pExpr :: ExprParser
pExpr = pLam <|> pTLam <|> pApp

-- pArith :: ExprParser
-- pArith = do
--   t <- pTerm
--   ts <- many (symbol "+" *> pTerm)
--   pure $ foldr (Plus ()) t ts

pTerm :: ExprParser
pTerm =
  choice
    [ pUnit,
      Var <$> pIdent <?> "identifier",
      parens pExpr
    ]
