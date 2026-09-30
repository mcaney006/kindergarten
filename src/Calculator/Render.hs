{-# LANGUAGE OverloadedStrings #-}

module Calculator.Render
  ( Notation (..)
  , Precision (..)
  , RenderedResult (..)
  , render
  )
where

import Control.DeepSeq (NFData)
import Data.List (genericSplitAt, unfoldr)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)

data Notation = DecimalNotation | FractionNotation
  deriving stock (Eq, Show, Enum, Bounded)

newtype Precision = Precision Natural
  deriving newtype (Eq, Ord, Show)

newtype RenderedResult = RenderedResult Text
  deriving newtype (Eq, Show, NFData)

render :: Notation -> Precision -> Rational -> RenderedResult
render notation precision value = RenderedResult $ case notation of
  DecimalNotation -> decimal precision value
  FractionNotation -> fraction value

fraction :: Rational -> Text
fraction value
  | denominator value == 1 = integer (numerator value)
  | otherwise = integer (numerator value) <> "/" <> integer (denominator value)

decimal :: Precision -> Rational -> Text
decimal (Precision places) value = sign <> integer whole <> point <> ellipsis
  where
    sign = if value < 0 then "-" else ""
    magnitude = abs value
    (whole, remainder) = numerator magnitude `quotRem` denominator magnitude
    (shown, omitted) = genericSplitAt places (expansion (denominator magnitude) remainder)
    point = if null shown then "" else "." <> foldMap integer shown
    ellipsis = if null omitted then "" else "…"

expansion :: Integer -> Integer -> [Integer]
expansion divisor = unfoldr next
  where
    next 0 = Nothing
    next remainder = Just ((remainder * 10) `quotRem` divisor)

integer :: Integer -> Text
integer = Text.pack . show
