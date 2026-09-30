{-# LANGUAGE DeriveAnyClass #-}

module Calculator.Interpreter
  ( EvaluationError (..)
  , MagnitudeBound (..)
  , interpret
  , evaluate
  )
where

import Calculator.Algebra
  ( NonZero
  , Ring (..)
  , Semiring (..)
  , divide
  , integerPower
  , nonZero
  )
import Calculator.Expression (Expr (..), Program (..))
import Calculator.Number (Carrier, Height (..), embed, widen)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import GHC.Generics (Generic)
import GHC.Num (integerToNatural)
import Numeric.Natural (Natural)

newtype MagnitudeBound = MagnitudeBound Natural
  deriving newtype (Eq, Ord, Show, NFData)

data EvaluationError
  = DivisionByZero
  | MagnitudeExceeded !MagnitudeBound
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

evaluate :: MagnitudeBound -> Program -> Either EvaluationError Rational
evaluate bound (Program sort expression) = widen sort <$> interpret bound expression

interpret :: MagnitudeBound -> Expr s -> Either EvaluationError (Carrier s)
interpret bound = go
  where
    go :: Expr t -> Either EvaluationError (Carrier t)
    go = \case
      NaturalLiteral n -> Right n
      RationalLiteral q -> Right q
      Embed embedding x -> embed embedding <$> go x
      Add x y -> plus <$> go x <*> go y
      Multiply x y -> times <$> go x <*> go y
      Subtract x y -> minus <$> go x <*> go y
      Negate x -> negation <$> go x
      Divide x y -> divide <$> go x <*> (go y >>= refuseZero)
      NaturalPower x n -> do
        base <- go x
        k <- go n
        admit bound (binaryHeight base) k
        Right (power base k)
      IntegerPower x n -> do
        base <- go x
        k <- go n
        admit bound (binaryHeight base) (integerToNatural k)
        maybe (Left DivisionByZero) Right (integerPower base k)

refuseZero :: (Eq a, Semiring a) => a -> Either EvaluationError (NonZero a)
refuseZero = maybe (Left DivisionByZero) Right . nonZero

admit :: MagnitudeBound -> Natural -> Natural -> Either EvaluationError ()
admit bound@(MagnitudeBound limit) height k =
  when (height * k > limit) (Left (MagnitudeExceeded bound))
