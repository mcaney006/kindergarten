module Calculator.Expression
  ( Expr (..)
  , Program (..)
  )
where

import Calculator.Algebra (Field, Ring, Semiring)
import Calculator.Number (Carrier, Embedding, Height, SSort, Sort (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Kind (Type)
import Numeric.Natural (Natural)

type Expr :: Sort -> Type
data Expr s where
  NaturalLiteral :: !Natural -> Expr 'N
  RationalLiteral :: !Rational -> Expr 'Q
  Embed :: !(Embedding a b) -> !(Expr a) -> Expr b
  Add :: (Semiring (Carrier s)) => !(Expr s) -> !(Expr s) -> Expr s
  Multiply :: (Semiring (Carrier s)) => !(Expr s) -> !(Expr s) -> Expr s
  Subtract :: (Ring (Carrier s)) => !(Expr s) -> !(Expr s) -> Expr s
  Negate :: (Ring (Carrier s)) => !(Expr s) -> Expr s
  Divide :: (Field (Carrier s)) => !(Expr s) -> !(Expr s) -> Expr s
  NaturalPower :: (Semiring (Carrier s), Height (Carrier s)) => !(Expr s) -> !(Expr 'N) -> Expr s
  IntegerPower :: (Field (Carrier s), Height (Carrier s)) => !(Expr s) -> !(Expr 'Z) -> Expr s

deriving stock instance Show (Expr s)

instance NFData (Expr s) where
  rnf = rwhnf

data Program where
  Program :: !(SSort s) -> !(Expr s) -> Program

deriving stock instance Show Program

instance NFData Program where
  rnf = rwhnf
