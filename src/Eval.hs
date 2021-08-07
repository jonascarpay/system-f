{-# LANGUAGE LambdaCase #-}

module Eval (eval, Value (..)) where

import Data.Bifunctor
import Data.Maybe
import Data.Void
import Expr
import Parse
import TypeCheck

close :: Expr () Ident -> Either String (Expr () Void)
close = first err . traverse Left
  where
    err var = "unbound variable: " <> show var

eval :: Expr () Ident -> Either String Value
eval expr = do
  expr' <- close expr
  nf (absurd <$> expr')

nf :: Expr () Value -> Either String Value
nf (Var _ val) = pure val
nf (Atom _ atom) = pure $ VAtom atom
nf (App _ f x) =
  nf f >>= \case
    (VClosure body) -> do
      x' <- nf x
      nf $ fromMaybe x' <$> body
    _ -> Left "not a function"
nf (Lam _ body) = pure (VClosure body)
nf (Plus _ l r) =
  nf l >>= \case
    (VAtom (AInt l')) ->
      nf r >>= \case
        (VAtom (AInt r')) -> pure (VAtom (AInt (l' + r')))
        _ -> Left "not a number"
    _ -> Left "not a number"

data Value
  = VAtom Atom
  | VClosure (Expr () (Maybe Value))
  deriving (Eq, Show)
