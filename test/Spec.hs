{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Expr
import Rebound
import Test.HUnit (assertFailure)
import Test.Hspec
import TypeCheck

assertEither :: Either String a -> IO a
assertEither = assertEitherWith id

assertEitherWith :: (err -> String) -> Either err a -> IO a
assertEitherWith f = either (assertFailure . f) pure

assertClosed :: (Show t, Show v) => Expr t v -> IO (Expr t' v')
assertClosed = assertEither . closeV >=> assertEither . closeT

assertTypClosed :: Show t => Type t -> IO (Type t')
assertTypClosed = assertEitherWith (mappend "unbound type variables in expected type: " . show) . closed

hasType :: Expr Char Char -> Type Char -> Expectation
hasType expr typ = do
  expr' <- assertClosed expr
  typ' <- assertTypClosed typ
  typGot <- assertEither $ typeOf expr'
  typGot `shouldBe` typ'

main :: IO ()
main = hspec $
  describe "type checking" $ do
    let idUnit = lam 'a' TUnit (Var 'a')
        idPoly = forall 'x' (lam 'a' (TVar 'x') $ Var 'a')
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

-- assertParse :: Parser a -> Text -> IO a
-- assertParse p t = assertEitherWith errorBundlePretty $ parse p "" t

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
