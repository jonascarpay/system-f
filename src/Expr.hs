{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

import Control.Monad
import Data.Bifunctor
import Data.Functor.Identity
import Lens.Micro.Platform

data Atom
  = AInt Int
  | ABool Bool
  deriving (Eq, Show)

data Expr t v
  = Var v
  | Unit
  | Lam (Type t) (Expr t (Maybe v))
  | App (Expr t v) (Expr t v)
  | TLam (Expr (Maybe t) v)
  | TApp (Expr t v) (Type t)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Type t
  = TVar t
  | TUnit
  | TArr (Type t) (Type t)
  | TForall (Type (Maybe t))
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Type where
  pure = TVar
  (<*>) = ap

instance Monad Type where
  TVar t >>= f = f t
  TUnit >>= _ = TUnit
  TArr a b >>= f = TArr (a >>= f) (b >>= f)
  TForall b >>= f = TForall (b >>= traverse f)

instance Bifunctor Expr where
  first = over exprTypes
  second = fmap
  bimap fl fr = runIdentity . exprT (Identity . fl) (Identity . fr)

{-# INLINE exprT #-}
exprT ::
  Applicative m =>
  (t -> m t') ->
  (v -> m v') ->
  (Expr t v -> m (Expr t' v'))
exprT ft fv = go
  where
    go (Var v) = Var <$> fv v
    go Unit = pure Unit
    go (Lam t b) = Lam <$> traverse ft t <*> exprT ft (traverse fv) b
    go (App f x) = App <$> go f <*> go x
    go (TLam body) = TLam <$> exprT (traverse ft) fv body
    go (TApp b t) = TApp <$> go b <*> traverse ft t

exprTypes :: Traversal (Expr t v) (Expr t' v) t t'
exprTypes f = exprT f pure

cap :: Eq a => a -> a -> Maybe a
cap a s -- s for scrutinee
  | a == s = Nothing
  | otherwise = Just s

lam :: Eq v => v -> Type t -> Expr t v -> Expr t v
lam v t body = Lam t (fmap (cap v) body)

tlam :: Eq t => t -> Expr t v -> Expr t v
tlam t = TLam . over exprTypes (cap t)
