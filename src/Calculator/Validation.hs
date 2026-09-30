module Calculator.Validation
  ( ValidationError (..)
  , validate
  )
where

import Calculator.Algebra (Ring, Semiring)
import Calculator.Expression (Expr (..), Program (..))
import Calculator.Number (Carrier, Embedding (..), Height, SSort (..), Sort (..))
import Calculator.Syntax (BinaryOperator (..), Numeral (..), Syntax (..), numeralValue)
import Control.DeepSeq (NFData)

newtype ValidationError = NonIntegralExponent Syntax
  deriving stock (Eq, Show)
  deriving newtype (NFData)

validate :: Syntax -> Either ValidationError Program
validate = \case
  Literal numeral -> Right (literal numeral)
  Group inner -> validate inner
  Negation inner -> negated <$> validate inner
  Binary operator left right -> do
    leftProgram <- validate left
    rightProgram <- validate right
    combine operator right leftProgram rightProgram

literal :: Numeral -> Program
literal numeral@(Numeral digits places)
  | places == 0 = Program SN (NaturalLiteral digits)
  | otherwise = Program SQ (RationalLiteral (numeralValue numeral))

negated :: Program -> Program
negated = \case
  Program SN e -> Program SZ (Negate (Embed NaturalInInteger e))
  Program SZ e -> Program SZ (Negate e)
  Program SQ e -> Program SQ (Negate e)

combine :: BinaryOperator -> Syntax -> Program -> Program -> Either ValidationError Program
combine operator rightSyntax left right = case operator of
  Addition -> Right (semiringOperation Add (unify left right))
  Multiplication -> Right (semiringOperation Multiply (unify left right))
  Subtraction -> Right (ringOperation Subtract (unify left right))
  Division -> Right (Program SQ (Divide (rational left) (rational right)))
  Exponentiation -> exponentiate rightSyntax left right

exponentiate :: Syntax -> Program -> Program -> Either ValidationError Program
exponentiate exponentSyntax base@(Program sort b) = \case
  Program SN n -> Right (withSemiring sort (Program sort (NaturalPower b n)))
  Program SZ n -> Right (Program SQ (IntegerPower (rational base) n))
  Program SQ _ -> Left (NonIntegralExponent exponentSyntax)

data Unified where
  Unified :: !(SSort s) -> !(Expr s) -> !(Expr s) -> Unified

unify :: Program -> Program -> Unified
unify (Program SN x) (Program SN y) = Unified SN x y
unify (Program SN x) (Program SZ y) = Unified SZ (Embed NaturalInInteger x) y
unify (Program SN x) (Program SQ y) = Unified SQ (Embed NaturalInRational x) y
unify (Program SZ x) (Program SN y) = Unified SZ x (Embed NaturalInInteger y)
unify (Program SZ x) (Program SZ y) = Unified SZ x y
unify (Program SZ x) (Program SQ y) = Unified SQ (Embed IntegerInRational x) y
unify (Program SQ x) (Program SN y) = Unified SQ x (Embed NaturalInRational y)
unify (Program SQ x) (Program SZ y) = Unified SQ x (Embed IntegerInRational y)
unify (Program SQ x) (Program SQ y) = Unified SQ x y

semiringOperation ::
  (forall s. (Semiring (Carrier s)) => Expr s -> Expr s -> Expr s) -> Unified -> Program
semiringOperation operation (Unified sort x y) = withSemiring sort (Program sort (operation x y))

ringOperation :: (forall s. (Ring (Carrier s)) => Expr s -> Expr s -> Expr s) -> Unified -> Program
ringOperation operation = \case
  Unified SN x y -> Program SZ (operation (Embed NaturalInInteger x) (Embed NaturalInInteger y))
  Unified SZ x y -> Program SZ (operation x y)
  Unified SQ x y -> Program SQ (operation x y)

rational :: Program -> Expr 'Q
rational = \case
  Program SN e -> Embed NaturalInRational e
  Program SZ e -> Embed IntegerInRational e
  Program SQ e -> e

withSemiring :: SSort s -> ((Semiring (Carrier s), Height (Carrier s)) => r) -> r
withSemiring sort evidence = case sort of
  SN -> evidence
  SZ -> evidence
  SQ -> evidence
