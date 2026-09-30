{-# LANGUAGE OverloadedStrings #-}

module Calculator.Diagnostic
  ( CaretAnchor (..)
  , diagnose
  )
where

import Calculator.Application (CalculatorError (..))
import Calculator.Interpreter (EvaluationError (..), MagnitudeBound (..))
import Calculator.Parser (SyntaxError (..))
import Calculator.Printer (printSyntax)
import Calculator.Syntax (SourceText (..))
import Calculator.Validation (ValidationError (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec (errorOffset, parseErrorTextPretty)

data CaretAnchor
  = UnderQuotedSource
  | UnderEchoedInput !Int
  deriving stock (Eq, Show)

diagnose :: CaretAnchor -> SourceText -> CalculatorError -> [Text]
diagnose anchor (SourceText source) = \case
  ParseFailure (SyntaxError failure) ->
    caret anchor source (errorOffset failure)
      <> ["error: " <> Text.intercalate "; " (Text.lines (Text.pack (parseErrorTextPretty failure)))]
  ValidationFailure (NonIntegralExponent exponentSyntax) ->
    ["error: exponent " <> printSyntax exponentSyntax <> " is not an integer expression"]
  EvaluationFailure DivisionByZero ->
    ["error: division by zero"]
  EvaluationFailure (MagnitudeExceeded (MagnitudeBound bits)) ->
    ["error: result would exceed the magnitude bound of " <> Text.pack (show bits) <> " bits"]

caret :: CaretAnchor -> Text -> Int -> [Text]
caret anchor source offset = case anchor of
  UnderQuotedSource -> [line, pointer 0]
  UnderEchoedInput margin -> [pointer margin]
  where
    (before, after) = Text.splitAt offset source
    prefix = Text.takeWhileEnd (/= '\n') before
    line = prefix <> Text.takeWhile (/= '\n') after
    pointer margin = Text.replicate (margin + Text.length prefix) " " <> "^"
