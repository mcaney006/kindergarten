module Calculator.Parser
  ( SyntaxError (..)
  , parseSyntax
  )
where

import Calculator.Syntax
  ( BinaryOperator
  , Fixity (..)
  , Numeral (..)
  , Precedence
  , SourceText (..)
  , Syntax (..)
  , fixity
  , lowestPrecedence
  , negationPrecedence
  , operandPrecedences
  , operatorSymbol
  )
import Control.Applicative (empty, optional)
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
  ( ParseError
  , Parsec
  , between
  , bundleErrors
  , choice
  , eof
  , label
  , lookAhead
  , match
  , runParser
  )
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

newtype SyntaxError = SyntaxError (ParseError Text Void)
  deriving stock (Eq, Show)
  deriving newtype (NFData)

parseSyntax :: SourceText -> Either SyntaxError Syntax
parseSyntax (SourceText text) =
  first (SyntaxError . NonEmpty.head . bundleErrors) (runParser program "" text)

program :: Parser Syntax
program = whitespace *> expression lowestPrecedence <* eof

expression :: Precedence -> Parser Syntax
expression threshold = operand >>= extend
  where
    extend left = do
      upcoming <- optional (lookAhead binaryOperator)
      case upcoming of
        Just operator
          | operatorFixity@(Fixity _ precedence) <- fixity operator
          , precedence >= threshold -> do
              _ <- binaryOperator
              right <- expression (snd (operandPrecedences operatorFixity))
              extend (Binary operator left right)
        _ -> pure left

operand :: Parser Syntax
operand =
  choice
    [ Literal <$> numeral
    , Group <$> between (symbol '(') (symbol ')') (expression lowestPrecedence)
    , Negation <$> (symbol '-' *> expression negationPrecedence)
    ]

numeral :: Parser Numeral
numeral = lexeme (label "number" (assemble <$> Lexer.decimal <*> optional fraction))
  where
    fraction = char '.' *> label "fractional digits" (match Lexer.decimal)
    assemble whole = \case
      Nothing -> Numeral whole 0
      Just (digits, part) ->
        let places = fromIntegral (Text.length digits)
         in Numeral (whole * 10 ^ places + part) places

binaryOperator :: Parser BinaryOperator
binaryOperator =
  label "operator" . lexeme $
    choice [operator <$ char (operatorSymbol operator) | operator <- [minBound .. maxBound]]

symbol :: Char -> Parser Char
symbol = lexeme . char

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

whitespace :: Parser ()
whitespace = Lexer.space space1 empty empty
