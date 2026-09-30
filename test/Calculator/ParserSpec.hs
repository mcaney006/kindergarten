{-# LANGUAGE OverloadedStrings #-}

module Calculator.ParserSpec
  ( spec
  )
where

import Calculator.Generators (stripGroups, syntax)
import Calculator.Parser (SyntaxError (..), parseSyntax)
import Calculator.Printer (printSyntax)
import Calculator.Syntax (BinaryOperator (..), Numeral (..), SourceText (..), Syntax (..))
import Data.Foldable (for_)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, (===))
import Text.Megaparsec (errorOffset)

spec :: Spec
spec = do
  describe "literals" $ do
    it "reads an integer" $
      parsed "42" `shouldBe` Right (whole 42)
    it "reads a decimal and keeps its scale" $
      parsed "0.050" `shouldBe` Right (Literal (Numeral 50 3))
    it "reads leading zeros as the same integer" $
      parsed "007" `shouldBe` Right (whole 7)
  describe "precedence" $ do
    it "binds multiplication tighter than addition" $
      parsed "1 + 2 * 3"
        `shouldBe` Right (Binary Addition (whole 1) (Binary Multiplication (whole 2) (whole 3)))
    it "binds division tighter than subtraction" $
      parsed "8 - 4 / 2"
        `shouldBe` Right (Binary Subtraction (whole 8) (Binary Division (whole 4) (whole 2)))
    it "binds exponentiation tighter than multiplication" $
      parsed "2 * 3 ^ 2"
        `shouldBe` Right (Binary Multiplication (whole 2) (Binary Exponentiation (whole 3) (whole 2)))
  describe "associativity" $ do
    it "groups subtraction to the left" $
      parsed "8 - 4 - 2"
        `shouldBe` Right (Binary Subtraction (Binary Subtraction (whole 8) (whole 4)) (whole 2))
    it "groups division to the left" $
      parsed "8 / 4 / 2"
        `shouldBe` Right (Binary Division (Binary Division (whole 8) (whole 4)) (whole 2))
    it "groups exponentiation to the right" $
      parsed "2 ^ 3 ^ 2"
        `shouldBe` Right (Binary Exponentiation (whole 2) (Binary Exponentiation (whole 3) (whole 2)))
  describe "negation" $ do
    it "binds looser than exponentiation" $
      parsed "-2 ^ 2" `shouldBe` Right (Negation (Binary Exponentiation (whole 2) (whole 2)))
    it "may appear in an exponent" $
      parsed "2 ^ -1" `shouldBe` Right (Binary Exponentiation (whole 2) (Negation (whole 1)))
    it "may follow a binary operator" $
      parsed "1 - -2" `shouldBe` Right (Binary Subtraction (whole 1) (Negation (whole 2)))
    it "nests" $
      parsed "--2" `shouldBe` Right (Negation (Negation (whole 2)))
  describe "parentheses" $ do
    it "override precedence" $
      parsed "(1 + 2) * 3"
        `shouldBe` Right (Binary Multiplication (Group (Binary Addition (whole 1) (whole 2))) (whole 3))
    it "may be redundant" $
      parsed "((7))" `shouldBe` Right (Group (Group (whole 7)))
  describe "whitespace" $ do
    it "is optional between tokens" $
      parsed "1+2*3" `shouldBe` parsed "1 + 2 * 3"
    it "may surround the expression and include tabs and newlines" $
      parsed " \t1 +\n 2 " `shouldBe` parsed "1 + 2"
  describe "invalid syntax" $
    for_ rejections $ \(input, offset) ->
      it ("rejects " <> show input <> " at offset " <> show offset) $
        offsetOf input `shouldBe` Just offset
  describe "printSyntax" $ do
    for_ minimal $ \text ->
      it ("prints " <> show text <> " with the parentheses it needs and no others") $
        fmap printSyntax (parsed text) `shouldBe` Right text
    for_ [("(2 ^ (-1))", "2 ^ -1"), ("((1 * 2)) + (3)", "1 * 2 + 3"), ("-(2 ^ 2)", "-2 ^ 2")] $
      \(text, canonical) ->
        it ("drops the redundant parentheses in " <> show text) $
          fmap (printSyntax . stripGroups) (parsed text) `shouldBe` Right canonical
    prop "reparses printed trees to the same tree" $
      forAll syntax $ \tree ->
        fmap stripGroups (parsed (printSyntax (stripGroups tree))) === Right (stripGroups tree)
    prop "printed text is a fixed point of parsing and printing" $
      forAll syntax $ \tree ->
        fmap printSyntax (parsed (printSyntax tree)) === Right (printSyntax tree)

minimal :: [Text]
minimal =
  [ "(2 ^ 3) ^ 2 - (1 - 2) - (-3) * 4"
  , "2 ^ -1"
  , "2 ^ -1 ^ 2"
  , "(-2) ^ 2"
  , "-(2 * 3)"
  , "3 * -2"
  , "1 - -2"
  , "--2"
  ]

rejections :: [(Text, Int)]
rejections =
  [ ("", 0)
  , ("1 +", 3)
  , ("(1", 2)
  , ("1)", 1)
  , ("1 2", 2)
  , ("1.", 2)
  , ("1..2", 2)
  , (".5", 0)
  , ("2 ** 3", 3)
  , ("abc", 0)
  , ("1 + * 2", 4)
  , ("()", 1)
  ]

parsed :: Text -> Either SyntaxError Syntax
parsed = parseSyntax . SourceText

offsetOf :: Text -> Maybe Int
offsetOf = either (\(SyntaxError failure) -> Just (errorOffset failure)) (const Nothing) . parsed

whole :: Natural -> Syntax
whole n = Literal (Numeral n 0)
