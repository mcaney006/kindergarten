{-# LANGUAGE OverloadedStrings #-}

module Calculator.RenderSpec
  ( spec
  )
where

import Calculator.Application (valueOf)
import Calculator.Interpreter (MagnitudeBound (..))
import Calculator.Render (Notation (..), Precision (..), RenderedResult (..), render)
import Calculator.Syntax (SourceText (..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (chooseInteger, forAll, (.&&.), (===))

spec :: Spec
spec = do
  describe "decimal notation" $
    for_ decimals $ \(places, value, expected) ->
      it (show value <> " at precision " <> show places <> " is " <> show expected) $
        rendered DecimalNotation places value `shouldBe` expected
  describe "fraction notation" $
    for_ [(140 / 23, "140/23"), (-(3 / 4), "-3/4"), (5, "5"), (0, "0")] $ \(value, expected) ->
      it (show value <> " is " <> show expected) $
        rendered FractionNotation 20 value `shouldBe` expected
  describe "properties" $ do
    prop "prints the truncated expansion and marks it exactly when digits are omitted" $
      \value -> forAll (fromInteger <$> chooseInteger (0, 30)) $ \places ->
        let text = rendered DecimalNotation places value
            shown = Text.stripSuffix "…" text
            truncated = fromInteger (truncate (value * 10 ^ places)) / 10 ^ places
         in parsedBack (fromMaybe text shown) === Right truncated
              .&&. isJust shown === (truncated /= value)
    prop "reads fractions back as the same rational" $ \value ->
      parsedBack (rendered FractionNotation 0 value) === Right value

decimals :: [(Natural, Rational, Text)]
decimals =
  [ (20, 3 / 10, "0.3")
  , (20, 7, "7")
  , (20, -7, "-7")
  , (20, 0, "0")
  , (20, -(1 / 8), "-0.125")
  , (20, 1 / 3, "0.33333333333333333333…")
  , (5, 2 / 3, "0.66666…")
  , (5, 1 / 1024, "0.00097…")
  , (10, 1 / 1024, "0.0009765625")
  , (0, 1 / 3, "0…")
  , (0, 5 / 2, "2…")
  , (0, 4, "4")
  , (20, 140 / 23, "6.08695652173913043478…")
  ]

rendered :: Notation -> Natural -> Rational -> Text
rendered notation places value =
  let RenderedResult text = render notation (Precision places) value in text

parsedBack :: Text -> Either String Rational
parsedBack = either (Left . show) Right . valueOf (MagnitudeBound 100_000) . SourceText
