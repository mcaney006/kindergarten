{-# LANGUAGE TypeFamilies #-}

module Calculator.Number
  ( Sort (..)
  , SSort (..)
  , demote
  , Carrier
  , Embedding (..)
  , embed
  , widen
  , Height (..)
  )
where

import Data.Kind (Type)
import Data.Ratio (denominator, numerator)
import GHC.Num (integerLog2)
import Numeric.Natural (Natural)

data Sort = N | Z | Q
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type SSort :: Sort -> Type
data SSort s where
  SN :: SSort 'N
  SZ :: SSort 'Z
  SQ :: SSort 'Q

deriving stock instance Show (SSort s)

demote :: SSort s -> Sort
demote = \case
  SN -> N
  SZ -> Z
  SQ -> Q

type Carrier :: Sort -> Type
type family Carrier s where
  Carrier 'N = Natural
  Carrier 'Z = Integer
  Carrier 'Q = Rational

type Embedding :: Sort -> Sort -> Type
data Embedding a b where
  NaturalInInteger :: Embedding 'N 'Z
  IntegerInRational :: Embedding 'Z 'Q
  NaturalInRational :: Embedding 'N 'Q

deriving stock instance Show (Embedding a b)

embed :: Embedding a b -> Carrier a -> Carrier b
embed = \case
  NaturalInInteger -> toInteger
  IntegerInRational -> fromInteger
  NaturalInRational -> fromIntegral

widen :: SSort s -> Carrier s -> Rational
widen = \case
  SN -> embed NaturalInRational
  SZ -> embed IntegerInRational
  SQ -> id

class Height a where
  binaryHeight :: a -> Natural

instance Height Integer where
  binaryHeight = fromIntegral . integerLog2 . abs

instance Height Natural where
  binaryHeight = binaryHeight . toInteger

instance Height Rational where
  binaryHeight q = binaryHeight (max (abs (numerator q)) (denominator q))
