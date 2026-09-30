module Calculator.NumberSpec
  ( spec
  )
where

import Calculator.Algebra (Semiring (..))
import Calculator.Number (Embedding (..), Height (..), embed)
import Numeric.Natural (Natural)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, chooseInt, conjoin, forAll, (===))

spec :: Spec
spec = do
  describe "embed" $ do
    prop "ℕ into ℤ is a semiring homomorphism" $
      homomorphism (embed NaturalInInteger)
    prop "ℤ into ℚ is a semiring homomorphism" $
      homomorphism (embed IntegerInRational)
    prop "ℕ into ℚ is a semiring homomorphism" $
      homomorphism (embed NaturalInRational)
    prop "the embeddings commute" $ \(n :: Natural) ->
      embed NaturalInRational n === embed IntegerInRational (embed NaturalInInteger n)
  describe "binaryHeight" $ do
    it "is zero at zero for every carrier" $
      (binaryHeight (0 :: Natural), binaryHeight (0 :: Integer), binaryHeight (0 :: Rational))
        `shouldBe` (0, 0, 0)
    it "is the floor of log2 for integers" $
      map binaryHeight [1, 2, 3, 1024, -8 :: Integer] `shouldBe` [0, 1, 1, 10, 3]
    it "takes the larger of numerator and denominator for rationals" $
      map binaryHeight [3 / 8, 1 / 2, -(9 / 4) :: Rational] `shouldBe` [3, 1, 3]
    prop "never overestimates the height of a power" $ \(x :: Rational) ->
      forAll (fromIntegral <$> chooseInt (0, 12)) $ \n ->
        binaryHeight (power x n) >= n * binaryHeight x

homomorphism :: (Semiring a, Semiring b, Eq b, Show b) => (a -> b) -> a -> a -> Property
homomorphism f x y =
  conjoin
    [ f (plus x y) === plus (f x) (f y)
    , f (times x y) === times (f x) (f y)
    , f zero === zero
    , f one === one
    ]
