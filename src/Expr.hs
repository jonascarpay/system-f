{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Expr where

data Atom
  = AInt Int
  | ABool Bool
  deriving (Eq, Show)

data Expr var
  = Var var
  | Atom Atom
  | Lam (Expr (Maybe var))
  | App (Expr var) (Expr var)
  | Plus (Expr var) (Expr var)
  deriving (Eq, Show, Functor, Foldable, Traversable)
