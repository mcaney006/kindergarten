{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Calculator.Algebra
  ( Semiring (..)
  , Ring (..)
  , Field (..)
  , NonZero (NonZero)
  , nonZero
  , divide
  , integerPower
  )
where

import GHC.Num (integerToNatural)
import Numeric.Natural (Natural)

class Semiring a where
  zero :: a
  one :: a
  plus :: a -> a -> a
  times :: a -> a -> a
  power :: a -> Natural -> a

class (Semiring a) => Ring a where
  negation :: a -> a
  minus :: a -> a -> a
  minus x y = plus x (negation y)

class (Eq a, Ring a) => Field a where
  reciprocal :: NonZero a -> NonZero a

newtype NonZero a = UncheckedNonZero a
  deriving stock (Eq, Show)

pattern NonZero :: a -> NonZero a
pattern NonZero x <- UncheckedNonZero x

{-# COMPLETE NonZero #-}

nonZero :: (Eq a, Semiring a) => a -> Maybe (NonZero a)
nonZero x
  | x == zero = Nothing
  | otherwise = Just (UncheckedNonZero x)

divide :: (Field a) => a -> NonZero a -> a
divide x (reciprocal -> NonZero inverse) = times x inverse

integerPower :: (Field a) => a -> Integer -> Maybe a
integerPower x n
  | n >= 0 = Just (power x magnitude)
  | otherwise = invert <$> nonZero x
  where
    magnitude = integerToNatural n
    invert (reciprocal -> NonZero inverse) = power inverse magnitude

instance Semiring Natural where
  zero = 0
  one = 1
  plus = (+)
  times = (*)
  power = (^)

instance Semiring Integer where
  zero = 0
  one = 1
  plus = (+)
  times = (*)
  power = (^)

instance Ring Integer where
  negation = negate
  minus = (-)

instance Semiring Rational where
  zero = 0
  one = 1
  plus = (+)
  times = (*)
  power = (^)

instance Ring Rational where
  negation = negate
  minus = (-)

instance Field Rational where
  reciprocal (NonZero x) = UncheckedNonZero (recip x)
