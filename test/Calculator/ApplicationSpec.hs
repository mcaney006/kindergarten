{-# LANGUAGE OverloadedStrings #-}

module Calculator.ApplicationSpec
  ( spec
  )
where

import Calculator.Application
  ( CalculatorError (..)
  , Configuration (..)
  , Judgement (..)
  , calculate
  , defaultConfiguration
  , judge
  )
import Calculator.Diagnostic (CaretAnchor (..), diagnose)
import Calculator.Interpreter (EvaluationError (..))
import Calculator.Number (Sort (..))
import Calculator.Render (Notation (..), RenderedResult (..))
import Calculator.Syntax (SourceText (..))
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "calculate" $ do
    for_ acceptance $ \(input, expected) ->
      it (show input <> " prints " <> show expected) $
        calculated defaultConfiguration input `shouldBe` Right expected
    it "reports division by zero as a structured error" $
      calculated defaultConfiguration "1 / 0" `shouldBe` Left (EvaluationFailure DivisionByZero)
    it "prints exact fractions on request" $
      calculated defaultConfiguration {notation = FractionNotation} "420 / 69" `shouldBe` Right "140/23"
  describe "judge" $
    for_ [("2 ^ 10", N), ("2 - 3", Z), ("0.1 + 0.2", Q)] $ \(input, expected) ->
      it (show input <> " has sort " <> show expected) $
        fmap (\(Judgement _ sort) -> sort) (judge (SourceText input)) `shouldBe` Right expected
  describe "diagnose" $ do
    it "quotes the source and points at a syntax error" $
      diagnosed UnderQuotedSource "1 + * 2"
        `shouldBe` ["1 + * 2", "    ^", "error: unexpected '*'; expecting '(', '-', or number"]
    it "points under echoed input without quoting it" $
      diagnosed (UnderEchoedInput 3) "1 +"
        `shouldBe` ["      ^", "error: unexpected end of input; expecting '(', '-', or number"]
    it "names the offending exponent" $
      diagnosed UnderQuotedSource "2 ^ (1 / 2)"
        `shouldBe` ["error: exponent (1 / 2) is not an integer expression"]
    it "reports division by zero" $
      diagnosed UnderQuotedSource "1 / (1 - 1)" `shouldBe` ["error: division by zero"]
    it "reports the magnitude bound" $
      diagnosed UnderQuotedSource "2 ^ 100001"
        `shouldBe` ["error: result would exceed the magnitude bound of 100000 bits"]

acceptance :: [(Text, Text)]
acceptance =
  [ ("2 + 2", "4")
  , ("2 + 3 * 4", "14")
  , ("(2 + 3) * 4", "20")
  , ("0.1 + 0.2", "0.3")
  , ("(8 * 4) / 2", "16")
  , ("1 + 2 * 3", "7")
  , ("-(10 - 3)", "-7")
  , ("420 / 69", "6.08695652173913043478…")
  , ("(21 * 2) + 27", "69")
  ]

calculated :: Configuration -> Text -> Either CalculatorError Text
calculated configuration input =
  (\(RenderedResult text) -> text) <$> calculate configuration (SourceText input)

diagnosed :: CaretAnchor -> Text -> [Text]
diagnosed anchor input =
  either (diagnose anchor source) (const []) (calculate defaultConfiguration source)
  where
    source = SourceText input
