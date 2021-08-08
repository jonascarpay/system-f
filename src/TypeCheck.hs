{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck where

import Control.Monad
import Expr
import Rebound

typeOf :: Eq t => Expr t (Type t) -> Either String (Type t)
typeOf (Var v) = pure v
typeOf Unit = pure TUnit
typeOf (Lam t b) = do
  tb <- typeOf $ instantiate1 t b
  pure $ TArr t tb
typeOf (App f x) =
  typeOf f >>= \case
    TArr tx ty -> do
      tx' <- typeOf x
      unless (tx == tx') $ Left "you booboo'd"
      pure ty
    _ -> Left "straight booboo"
typeOf (TLam b) = do
  tb <- typeOf $ (fmap . fmap) Free b
  pure $ TForall tb
typeOf (TApp b t) =
  typeOf b >>= \case
    TForall tb -> pure $ tb >>= unbind (const t) TVar
    _ -> Left "that's a nono"
