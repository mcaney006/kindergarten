{-# LANGUAGE DeriveAnyClass #-}

module Calculator.Syntax
  ( SourceText (..)
  , Syntax (..)
  , Numeral (..)
  , numeralValue
  , BinaryOperator (..)
  , operatorSymbol
  , Associativity (..)
  , Precedence
  , lowestPrecedence
  , negationPrecedence
  , Fixity (..)
  , fixity
  , operandPrecedences
  )
where

import Control.DeepSeq (NFData)
import Data.Ratio ((%))
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

newtype SourceText = SourceText Text
  deriving newtype (Eq, Show, NFData)

data Numeral = Numeral
  { unscaled :: !Natural
  , scale :: !Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

numeralValue :: Numeral -> Rational
numeralValue (Numeral digits places) = toInteger digits % (10 ^ places)

data BinaryOperator
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Exponentiation
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

operatorSymbol :: BinaryOperator -> Char
operatorSymbol = \case
  Addition -> '+'
  Subtraction -> '-'
  Multiplication -> '*'
  Division -> '/'
  Exponentiation -> '^'

data Syntax
  = Literal !Numeral
  | Negation !Syntax
  | Binary !BinaryOperator !Syntax !Syntax
  | Group !Syntax
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Associativity = LeftAssociative | RightAssociative
  deriving stock (Eq, Show)

newtype Precedence = Precedence Natural
  deriving newtype (Eq, Ord, Show)

lowestPrecedence :: Precedence
lowestPrecedence = Precedence 0

negationPrecedence :: Precedence
negationPrecedence = Precedence 4

data Fixity = Fixity !Associativity !Precedence
  deriving stock (Eq, Show)

fixity :: BinaryOperator -> Fixity
fixity = \case
  Addition -> Fixity LeftAssociative additive
  Subtraction -> Fixity LeftAssociative additive
  Multiplication -> Fixity LeftAssociative multiplicative
  Division -> Fixity LeftAssociative multiplicative
  Exponentiation -> Fixity RightAssociative exponential
  where
    additive = Precedence 1
    multiplicative = Precedence 2
    exponential = Precedence 4

operandPrecedences :: Fixity -> (Precedence, Precedence)
operandPrecedences (Fixity associativity precedence@(Precedence level)) = case associativity of
  LeftAssociative -> (precedence, tighter)
  RightAssociative -> (tighter, precedence)
  where
    tighter = Precedence (level + 1)
