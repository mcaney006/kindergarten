{-# LANGUAGE OverloadedStrings #-}

module Calculator.InterpreterSpec
  ( spec
  )
where

import Calculator.Application (CalculatorError (..), valueOf)
import Calculator.Generators (regroup, stripGroups, syntax)
import Calculator.Interpreter (EvaluationError (..), MagnitudeBound (..), evaluate)
import Calculator.Oracle (reference)
import Calculator.Printer (printSyntax)
import Calculator.Syntax (SourceText (..), Syntax)
import Calculator.Validation (ValidationError (..), validate)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Property, classify, discard, forAll, property, (===))

spec :: Spec
spec = do
  describe "evaluation" $
    for_ values $ \(input, expected) ->
      it (show input <> " = " <> show expected) $
        value input `shouldBe` Right expected
  describe "division by zero" $
    for_ ["1 / 0", "1 / (2 - 2)", "0 / 0", "0 ^ -1", "(1 - 1) ^ -3"] $ \input ->
      it ("rejects " <> show input) $
        value input `shouldBe` Left (EvaluationFailure DivisionByZero)
  describe "magnitude bound" $ do
    it "admits a power whose height meets the bound" $
      value "2 ^ 100000" `shouldBe` Right (2 ^ (100_000 :: Integer))
    for_ ["2 ^ 100001", "(1 / 2) ^ 100001", "2 ^ -100001", "(2 ^ 1000) ^ 1000"] $ \input ->
      it ("refuses " <> show input) $
        value input `shouldBe` Left (EvaluationFailure (MagnitudeExceeded bound))
    for_ [("0 ^ 1000000000000", 0), ("1 ^ 1000000000000", 1), ("(-1) ^ 1000000000001", -1)] $
      \(input, expected) ->
        it ("admits " <> show input <> " because its height is zero") $
          value input `shouldBe` Right expected
  describe "properties" $ do
    modifyMaxSuccess (const 1000)
      $ prop "agrees with a direct rational evaluator on every well-sorted program"
      $ forAll syntax agreesWithReference
    prop "is invariant under redundant parentheses" $
      forAll syntax $ \tree -> forAll (regroup tree) $ \regrouped ->
        outcome (printSyntax regrouped) === outcome (printSyntax tree)

agreesWithReference :: Syntax -> Property
agreesWithReference tree = case validate tree of
  Left _ -> classify True "rejected by validation" (property True)
  Right program -> case evaluate bound program of
    Right result -> reference tree === Just result
    Left DivisionByZero -> reference tree === Nothing
    Left (MagnitudeExceeded _) -> discard

values :: [(Text, Rational)]
values =
  [ ("2 + 2", 4)
  , ("(8 * 4) / 2", 16)
  , ("1 + 2 * 3", 7)
  , ("-(10 - 3)", -7)
  , ("420 / 69", 140 / 23)
  , ("0.1 + 0.2", 3 / 10)
  , ("2 ^ 3 ^ 2", 512)
  , ("(2 ^ 3) ^ 2", 64)
  , ("-2 ^ 2", -4)
  , ("(-2) ^ 2", 4)
  , ("2 ^ -1", 1 / 2)
  , ("2 ^ (1 - 3)", 1 / 4)
  , ("0 ^ 0", 1)
  , ("1 - 2 - 3", -4)
  , ("8 / 4 / 2", 1)
  , ("0.5 ^ 3", 1 / 8)
  ]

bound :: MagnitudeBound
bound = MagnitudeBound 100_000

value :: Text -> Either CalculatorError Rational
value = valueOf bound . SourceText

outcome :: Text -> Either CalculatorError Rational
outcome = first ungroup . value
  where
    ungroup = \case
      ValidationFailure (NonIntegralExponent exponentSyntax) ->
        ValidationFailure (NonIntegralExponent (stripGroups exponentSyntax))
      failure -> failure
