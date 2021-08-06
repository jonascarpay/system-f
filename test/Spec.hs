{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Eval
import Expr
import Parse
import Test.HUnit (assertFailure)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, parse)

assertEitherWith :: (err -> String) -> Either err a -> IO a
assertEitherWith f = either (assertFailure . f) pure

assertParse :: Parser a -> Text -> IO a
assertParse p t = assertEitherWith errorBundlePretty $ parse p "" t

assertEval :: Expr Ident -> IO Value
assertEval = assertEitherWith (mappend "evaluation error: ") . eval

it9 :: String -> Text -> Spec
it9 label text = it label $ do
  expr <- assertParse exprParser text
  res <- assertEval expr
  res `shouldBe` VAtom (AInt 9)

main :: IO ()
main =
  hspec $
    describe "systemf-test" $ do
      it9 "parses constants" "9"
      it9 "identity" "(λx. x) 9"
      it9 "const" "(λx. λy. x) 9 10"
      it9 "multiple arguments" "(λx y. x) 9 10"
      it9 "plus" "4 + 5"
      it9 "plus lambda" "(λa b. a + b) 2 7"
