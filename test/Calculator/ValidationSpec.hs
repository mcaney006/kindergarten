{-# LANGUAGE OverloadedStrings #-}

module Calculator.ValidationSpec
  ( spec
  )
where

import Calculator.Expression (Program (..))
import Calculator.Number (Sort (..), demote)
import Calculator.Parser (parseSyntax)
import Calculator.Syntax (BinaryOperator (..), Numeral (..), SourceText (..), Syntax (..))
import Calculator.Validation (ValidationError (..), validate)
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "sort inference" $
    for_ sorts $ \(input, expected) ->
      it (show input <> " : " <> show expected) $
        sortOf input `shouldBe` Right expected
  describe "exponents" $ do
    it "rejects a decimal exponent" $
      rejection "2 ^ 0.5" `shouldBe` Just (NonIntegralExponent (Literal (Numeral 5 1)))
    it "rejects a quotient as an exponent even when it is integral" $
      rejection "2 ^ (4 / 2)"
        `shouldBe` Just (NonIntegralExponent (Group (Binary Division (Literal (Numeral 4 0)) (Literal (Numeral 2 0)))))
    it "rejects a negative power as an exponent" $
      rejection "2 ^ (2 ^ -1)"
        `shouldBe` Just
          ( NonIntegralExponent
              (Group (Binary Exponentiation (Literal (Numeral 2 0)) (Negation (Literal (Numeral 1 0)))))
          )
    it "accepts a tower of natural powers" $
      sortOf "2 ^ 2 ^ 3" `shouldBe` Right N

sorts :: [(Text, Sort)]
sorts =
  [ ("2 + 3", N)
  , ("2 * 3", N)
  , ("2 - 3", Z)
  , ("-2", Z)
  , ("1 / 2", Q)
  , ("4 / 2", Q)
  , ("0.5", Q)
  , ("2.0", Q)
  , ("2 ^ 3", N)
  , ("(-2) ^ 3", Z)
  , ("2 ^ -1", Q)
  , ("0.5 ^ 2", Q)
  , ("1 + 2 - 3 * 4", Z)
  , ("(1 - 2) * 0.5", Q)
  ]

sortOf :: Text -> Either String Sort
sortOf input = case parseSyntax (SourceText input) of
  Left failure -> Left (show failure)
  Right tree -> case validate tree of
    Left failure -> Left (show failure)
    Right (Program sort _) -> Right (demote sort)

rejection :: Text -> Maybe ValidationError
rejection input = case parseSyntax (SourceText input) of
  Left _ -> Nothing
  Right tree -> either Just (const Nothing) (validate tree)
