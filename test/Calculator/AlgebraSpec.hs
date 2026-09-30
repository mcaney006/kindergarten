{-# LANGUAGE AllowAmbiguousTypes #-}

module Calculator.AlgebraSpec
  ( spec
  )
where

import Calculator.Algebra
  ( NonZero (NonZero)
  , Ring (..)
  , Semiring (..)
  , divide
  , integerPower
  , nonZero
  )
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , chooseInt
  , chooseInteger
  , forAll
  , suchThatMap
  , (===)
  )

spec :: Spec
spec = do
  describe "Natural" $
    semiringLaws @Natural
  describe "Integer" $ do
    semiringLaws @Integer
    ringLaws @Integer
  describe "Rational" $ do
    semiringLaws @Rational
    ringLaws @Rational
    fieldLaws
  describe "integerPower" $ do
    it "refuses negative powers of zero" $
      integerPower (0 :: Rational) (-1) `shouldBe` Nothing
    it "takes zero to the zeroth power to one" $
      integerPower (0 :: Rational) 0 `shouldBe` Just 1
    it "inverts for negative exponents" $
      integerPower (2 :: Rational) (-3) `shouldBe` Just (1 / 8)

semiringLaws :: forall a. (Semiring a, Eq a, Show a, Arbitrary a) => Spec
semiringLaws = describe "semiring laws" $ do
  prop "x + 0 = x" $ \(x :: a) -> plus x zero === x
  prop "x * 1 = x" $ \(x :: a) -> times x one === x
  prop "x + y = y + x" $ \(x :: a) y -> plus x y === plus y x
  prop "x * y = y * x" $ \(x :: a) y -> times x y === times y x
  prop "(x + y) + z = x + (y + z)" $ \(x :: a) y z -> plus (plus x y) z === plus x (plus y z)
  prop "(x * y) * z = x * (y * z)" $ \(x :: a) y z -> times (times x y) z === times x (times y z)
  prop "x * (y + z) = x * y + x * z" $ \(x :: a) y z ->
    times x (plus y z) === plus (times x y) (times x z)
  prop "x * 0 = 0" $ \(x :: a) -> times x zero === zero
  prop "x ^ 0 = 1" $ \(x :: a) -> power x 0 === one
  prop "x ^ (m + n) = x ^ m * x ^ n" $ \(x :: a) ->
    forAll smallExponent $ \m -> forAll smallExponent $ \n ->
      power x (m + n) === times (power x m) (power x n)

ringLaws :: forall a. (Ring a, Eq a, Show a, Arbitrary a) => Spec
ringLaws = describe "ring laws" $ do
  prop "x - x = 0" $ \(x :: a) -> minus x x === zero
  prop "x + (-x) = 0" $ \(x :: a) -> plus x (negation x) === zero
  prop "x - y = x + (-y)" $ \(x :: a) y -> minus x y === plus x (negation y)
  prop "-(-x) = x" $ \(x :: a) -> negation (negation x) === x

fieldLaws :: Spec
fieldLaws = describe "field laws" $ do
  prop "x / 1 = x" $ \(x :: Rational) -> (divide x <$> nonZero one) === Just x
  prop "x / x = 1 when x /= 0" $ forAll nonZeroRational $ \divisor@(NonZero x) ->
    divide x divisor === one
  prop "(x / y) * y = x when y /= 0" $ \(x :: Rational) ->
    forAll nonZeroRational $ \divisor@(NonZero y) -> times (divide x divisor) y === x
  prop "x ^ -n * x ^ n = 1 when x /= 0" $ forAll nonZeroRational $ \(NonZero x) ->
    forAll (chooseInteger (0, 8)) $ \n ->
      (times <$> integerPower x (negate n) <*> integerPower x n) === Just one

nonZeroRational :: Gen (NonZero Rational)
nonZeroRational = arbitrary `suchThatMap` nonZero

smallExponent :: Gen Natural
smallExponent = fromIntegral <$> chooseInt (0, 8)
