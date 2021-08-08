{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Void
import Expr
import Lens.Micro.Platform
import Parse
import Rebound
import Test.HUnit (assertFailure)
import Test.Hspec
import Text.Megaparsec
import TypeCheck

assertEither :: Either String a -> IO a
assertEither = assertEitherWith id

assertEitherWith :: (err -> String) -> Either err a -> IO a
assertEitherWith f = either (assertFailure . f) pure

assertClosedOver :: Show a => Traversal s t a b -> s -> IO t
assertClosedOver t = assertEitherWith (mappend "Unbound variables: " . show . toList) . closedOver t

assertClosedExpr :: (Show t, Show v) => Expr t v -> IO (Expr t' v')
assertClosedExpr = assertClosedOver exprTypes >=> assertClosedOver traverse

assertClosedTyp :: Show t => Type t -> IO (Type t')
assertClosedTyp = assertClosedOver traverse

assertParse :: Parser a -> Text -> IO a
assertParse p text = assertEitherWith errorBundlePretty $ parse p "" text

assertParseExpr :: Text -> IO (Expr Ident Ident)
assertParseExpr = assertParse exprParser

assertParseType :: Text -> IO (Type Ident)
assertParseType = assertParse typeParser

parsesTo :: (Eq a, Show a) => Parser a -> Text -> a -> Expectation
parsesTo p text exp = do
  got <- assertParse p text
  got `shouldBe` exp

parsesToTyp :: Text -> Type Ident -> Expectation
parsesToTyp = parsesTo typeParser

hasType :: Expr Ident Ident -> Type Ident -> Expectation
hasType expr typ = do
  expr' <- assertClosedExpr expr
  typ' <- assertClosedTyp typ
  typGot :: Type Void <- assertEither $ typeOf expr'
  typGot `shouldBe` typ'

hasTypeP :: Text -> Text -> Expectation
hasTypeP exprText typeText = do
  expr <- assertParseExpr exprText
  typ <- assertParseType typeText
  expr `hasType` typ

main :: IO ()
main = hspec $ do
  describe "embedded expression type checking" $ do
    let idUnit = lam "a" TUnit (Var "a")
        idPoly = forall "x" (lam "a" (TVar "x") $ Var "a")
        idPolyT = TForall (TArr (TVar (Bound ())) (TVar (Bound ())))
    it "unit" $
      Unit `hasType` TUnit
    it "monomorphic identity" $
      idUnit `hasType` TArr TUnit TUnit
    it "arrow applied" $
      App idUnit Unit `hasType` TUnit
    it "polymorphic identity" $
      idPoly `hasType` idPolyT
    it "polymorphic identity instantiated" $
      TApp idPoly TUnit `hasType` TArr TUnit TUnit
  describe "parse tests" $ do
    it "unit" $
      "()" `parsesToTyp` TUnit
    it "var" $
      "x" `parsesToTyp` TVar "x"
    it "arrow" $
      "x -> y" `parsesToTyp` TArr (TVar "x") (TVar "y")
    it "arrow arrow" $
      "x -> y -> z" `parsesToTyp` TArr (TVar "x") (TArr (TVar "y") (TVar "z"))
    it "arrow arrow arrow arrow" $
      "(x -> y) -> x -> y -> z" `parsesToTyp` TArr (TArr (TVar "x") (TVar "y")) (TArr (TVar "x") (TArr (TVar "y") (TVar "z")))
    it "poly" $
      "∀ x. x" `parsesToTyp` TForall (TVar (Bound ()))
    it "const" $
      "∀ x. (∀ y. x)" `parsesToTyp` TForall (TForall (TVar (Free (Bound ()))))
    it "const, multi-arg" $
      "∀ x y. x" `parsesToTyp` TForall (TForall (TVar (Free (Bound ()))))
  describe "parsed type checking" $ do
    it "unit" $
      "()" `hasTypeP` "()"
    it "monomorphic identity" $
      "λ (x:()). x" `hasTypeP` "() -> ()"
    it "arrow applied" $
      "(λ (x:()). x) ()" `hasTypeP` "()"
    it "polymorphic identity" $
      "Λ X. λ (x:X). x" `hasTypeP` "∀ Y. Y -> Y"
    it "polymorphic const" $
      "Λ X Y. λ (x:X) (y:Y). x" `hasTypeP` "∀ A B. A -> B -> A"
    it "polymorphic identity instantiated with ()" $
      "(Λ X. λ (x:X). x) @()" `hasTypeP` "() -> ()"
    it "polymorphic identity instantiated with higher order type" $
      "(Λ X. λ (x:X). x) @(∀ x y. x -> y)" `hasTypeP` "(∀ a b. a -> b) -> (∀ c d. c -> d)"
    it "Rank N identity" $
      "λ (x: ∀ x. x -> x). x" `hasTypeP` "(∀ a. a -> a) -> (∀ b. b -> b)"
    it "two" $
      "Λ X. λ (f: X -> X) (x: X). f (f x)" `hasTypeP` "∀ X. (X -> X) -> X -> X"
    it "plus" $
      "Λ X. λ (l: (X -> X) -> X -> X) (r: (X -> X) -> X -> X) (succ: X -> X) (z: X). l succ (r succ z)" `hasTypeP` "∀ X. ((X -> X) -> X -> X) -> ((X -> X) -> X -> X) -> ((X -> X) -> X -> X)"

-- assertEval :: Expr () Ident -> IO Value
-- assertEval = assertEitherWith (mappend "evaluation error: ") . eval

-- it9 :: String -> Text -> Spec
-- it9 label text = it label $ do
--   expr <- assertParse exprParser text
--   res <- assertEval expr
--   res `shouldBe` VAtom (AInt 9)

-- main :: IO ()
-- main =
--   hspec $
--     describe "systemf-test" $ do
--       it9 "parses constants" "9"
--       it9 "identity" "(λx. x) 9"
--       it9 "const" "(λx. λy. x) 9 10"
--       it9 "multiple arguments" "(λx y. x) 9 10"
--       it9 "plus" "4 + 5"
--       it9 "plus lambda" "(λa b. a + b) 2 7"
