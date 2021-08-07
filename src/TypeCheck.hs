{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Void
import Expr
import Lens.Micro.Platform
import Validation

closeV :: Expr t v -> Either (NonEmpty v) (Expr t v')
closeV = validationToEither . exprT pure failure

closeT :: Expr t v -> Either (NonEmpty t) (Expr t' v)
closeT = validationToEither . exprT failure pure

tid :: Expr Char Char
tid = tlam 'a' $ lam 'x' (TVar 'a') $ Var 'x'

typeOf :: Expr Void Void -> Either String (Type Void)
typeOf = go . bimap absurd absurd
  where
    go :: Eq t => Expr t (Type t) -> Either String (Type t)
    go (Var v) = pure v
    go Unit = pure TUnit
    go (Lam t b) = do
      tb <- go $ fmap (fromMaybe t) b
      pure $ TArr t tb
    go (App f x) =
      go f >>= \case
        TArr tx ty -> do
          tx' <- go x
          unless (tx == tx') $ Left "you booboo'd"
          pure ty
        _ -> Left "straight booboo"
    go (TLam b) = do
      tb <- go $ (fmap . fmap) Just b
      pure $ TForall tb
    go (TApp b t) =
      go b >>= \case
        TForall tb -> pure $ tb >>= maybe t TVar
        _ -> Left "that's a nono"
