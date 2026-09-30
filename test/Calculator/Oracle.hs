module Calculator.Oracle
  ( reference
  )
where

import Calculator.Syntax (BinaryOperator (..), Syntax (..), numeralValue)
import Data.Ratio (denominator, numerator)

reference :: Syntax -> Maybe Rational
reference = \case
  Literal numeral -> Just (numeralValue numeral)
  Group inner -> reference inner
  Negation inner -> negate <$> reference inner
  Binary operator left right -> do
    x <- reference left
    y <- reference right
    apply operator x y

apply :: BinaryOperator -> Rational -> Rational -> Maybe Rational
apply = \case
  Addition -> \x y -> Just (x + y)
  Subtraction -> \x y -> Just (x - y)
  Multiplication -> \x y -> Just (x * y)
  Division -> \x y -> if y == 0 then Nothing else Just (x / y)
  Exponentiation -> \x y ->
    if denominator y /= 1 || (x == 0 && y < 0) then Nothing else Just (x ^^ numerator y)
