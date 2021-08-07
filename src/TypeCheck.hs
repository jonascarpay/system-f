{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck where

import Control.Monad
import Data.Bifunctor
import Data.Void
import Expr
import Rebound

closeV :: Show v => Expr t v -> Either String (Expr t v')
closeV = first (mappend "unbound variables: " . show) . closedOver traverse

closeT :: Show t => Expr t v -> Either String (Expr t' v)
closeT = first (mappend "unbound type variables: " . show) . closedOver exprTypes

typeOf :: Expr Void Void -> Either String (Type Void)
typeOf = go . bimap absurd absurd
  where
    go :: Eq t => Expr t (Type t) -> Either String (Type t)
    go (Var v) = pure v
    go Unit = pure TUnit
    go (Lam t b) = do
      tb <- go $ instantiate1 t b
      pure $ TArr t tb
    go (App f x) =
      go f >>= \case
        TArr tx ty -> do
          tx' <- go x
          unless (tx == tx') $ Left "you booboo'd"
          pure ty
        _ -> Left "straight booboo"
    go (TLam b) = do
      tb <- go $ (fmap . fmap) Free b
      pure $ TForall tb
    go (TApp b t) =
      go b >>= \case
        TForall tb -> pure $ tb >>= unbind (const t) TVar
        _ -> Left "that's a nono"
