module Calculator.Generators
  ( natural
  , numeral
  , syntax
  , regroup
  , stripGroups
  )
where

import Calculator.Syntax (BinaryOperator (..), Numeral (..), Syntax (..))
import Numeric.Natural (Natural)
import Test.QuickCheck (Gen, chooseInteger, elements, frequency, sized)

natural :: Integer -> Gen Natural
natural upper = fromInteger <$> chooseInteger (0, upper)

numeral :: Gen Numeral
numeral = Numeral <$> natural 1_000_000 <*> natural 4

syntax :: Gen Syntax
syntax = sized (tree . min 40)
  where
    tree budget
      | budget <= 1 = Literal <$> numeral
      | otherwise =
          frequency
            [ (2, Literal <$> numeral)
            , (1, Negation <$> tree (budget - 1))
            , (1, Group <$> tree (budget - 1))
            , (4, Binary <$> elements arithmetic <*> tree half <*> tree half)
            , (1, Binary Exponentiation <$> tree half <*> exponentSyntax)
            ]
      where
        half = budget `div` 2
    arithmetic = [Addition, Subtraction, Multiplication, Division]
    whole = Literal . flip Numeral 0 <$> natural 2
    exponentSyntax =
      frequency
        [ (4, whole)
        , (2, Negation <$> whole)
        , (1, Literal <$> numeral)
        ]

regroup :: Syntax -> Gen Syntax
regroup node = do
  inner <- case node of
    Literal n -> pure (Literal n)
    Negation x -> Negation <$> regroup x
    Binary operator x y -> Binary operator <$> regroup x <*> regroup y
    Group x -> Group <$> regroup x
  frequency [(3, pure inner), (1, pure (Group inner))]

stripGroups :: Syntax -> Syntax
stripGroups = \case
  Literal n -> Literal n
  Negation x -> Negation (stripGroups x)
  Binary operator x y -> Binary operator (stripGroups x) (stripGroups y)
  Group x -> stripGroups x
