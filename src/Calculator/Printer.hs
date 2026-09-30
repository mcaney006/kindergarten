{-# LANGUAGE OverloadedStrings #-}

module Calculator.Printer
  ( printSyntax
  )
where

import Calculator.Syntax
  ( Fixity (..)
  , Numeral (..)
  , Precedence
  , Syntax (..)
  , fixity
  , lowestPrecedence
  , negationPrecedence
  , operandPrecedences
  , operatorSymbol
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder

printSyntax :: Syntax -> Text
printSyntax = Lazy.toStrict . Builder.toLazyText . printAt lowestPrecedence

printAt :: Precedence -> Syntax -> Builder
printAt context = \case
  Literal numeral -> printNumeral numeral
  Group inner -> parenthesized (printAt lowestPrecedence inner)
  Negation inner ->
    parenthesizedAbove negationPrecedence ("-" <> printAt negationPrecedence inner)
  Binary operator left right ->
    let operatorFixity@(Fixity _ precedence) = fixity operator
        (leftContext, rightContext) = operandPrecedences operatorFixity
     in parenthesizedAbove precedence $
          printAt leftContext left
            <> " "
            <> Builder.singleton (operatorSymbol operator)
            <> " "
            <> printAt rightContext right
  where
    parenthesizedAbove precedence builder
      | context > precedence = parenthesized builder
      | otherwise = builder

parenthesized :: Builder -> Builder
parenthesized builder = "(" <> builder <> ")"

printNumeral :: Numeral -> Builder
printNumeral (Numeral digits places)
  | places == 0 = Builder.fromText padded
  | otherwise = Builder.fromText whole <> "." <> Builder.fromText fraction
  where
    width = fromIntegral places
    padded = Text.justifyRight (width + 1) '0' (Text.pack (show digits))
    (whole, fraction) = Text.splitAt (Text.length padded - width) padded
