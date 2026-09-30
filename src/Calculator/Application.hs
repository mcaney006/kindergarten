{-# LANGUAGE DeriveAnyClass #-}

module Calculator.Application
  ( Configuration (..)
  , defaultConfiguration
  , CalculatorError (..)
  , Judgement (..)
  , elaborate
  , valueOf
  , calculate
  , judge
  )
where

import Calculator.Expression (Program (..))
import Calculator.Interpreter (EvaluationError, MagnitudeBound (..), evaluate)
import Calculator.Number (Sort, demote)
import Calculator.Parser (SyntaxError, parseSyntax)
import Calculator.Render (Notation (..), Precision (..), RenderedResult, render)
import Calculator.Syntax (SourceText, Syntax)
import Calculator.Validation (ValidationError, validate)
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import GHC.Generics (Generic)

data Configuration = Configuration
  { notation :: !Notation
  , precision :: !Precision
  , magnitudeBound :: !MagnitudeBound
  }
  deriving stock (Eq, Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { notation = DecimalNotation
    , precision = Precision 20
    , magnitudeBound = MagnitudeBound 100_000
    }

data CalculatorError
  = ParseFailure !SyntaxError
  | ValidationFailure !ValidationError
  | EvaluationFailure !EvaluationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Judgement = Judgement !Syntax !Sort
  deriving stock (Eq, Show)

elaborate :: SourceText -> Either CalculatorError (Syntax, Program)
elaborate source = do
  syntax <- first ParseFailure (parseSyntax source)
  program <- first ValidationFailure (validate syntax)
  pure (syntax, program)

valueOf :: MagnitudeBound -> SourceText -> Either CalculatorError Rational
valueOf bound source = do
  (_, program) <- elaborate source
  first EvaluationFailure (evaluate bound program)

calculate :: Configuration -> SourceText -> Either CalculatorError RenderedResult
calculate configuration =
  fmap (render (notation configuration) (precision configuration))
    . valueOf (magnitudeBound configuration)

judge :: SourceText -> Either CalculatorError Judgement
judge source = do
  (syntax, Program sort _) <- elaborate source
  pure (Judgement syntax (demote sort))
