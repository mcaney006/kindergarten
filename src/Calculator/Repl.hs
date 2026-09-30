{-# LANGUAGE OverloadedStrings #-}

module Calculator.Repl
  ( Session
  , openSession
  , Entry (..)
  , Command (..)
  , parseCommand
  , Response (..)
  , Transition (..)
  , step
  , Interaction (..)
  , present
  , prompt
  , Terminal (..)
  , runRepl
  )
where

import Calculator.Application
  ( CalculatorError
  , Configuration
  , Judgement (..)
  , calculate
  , judge
  )
import Calculator.Diagnostic (CaretAnchor (..), diagnose)
import Calculator.Number (Sort (..))
import Calculator.Printer (printSyntax)
import Calculator.Render (RenderedResult (..))
import Calculator.Syntax (SourceText (..))
import Data.Char (isSpace)
import Data.Foldable (toList, traverse_)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text qualified as Text

data Session = Session !Configuration !(Seq Entry)
  deriving stock (Eq, Show)

data Entry = Entry !SourceText !RenderedResult
  deriving stock (Eq, Show)

openSession :: Configuration -> Session
openSession configuration = Session configuration mempty

data Command
  = Evaluate !SourceText
  | TypeOf !SourceText
  | ShowHistory
  | ShowHelp
  | Quit
  | Unrecognized !Text
  | Blank
  deriving stock (Eq, Show)

parseCommand :: Text -> Command
parseCommand line = case Text.uncons (Text.stripStart line) of
  Nothing -> Blank
  Just (':', directive) -> directiveCommand (Text.break isSpace directive)
  Just _ -> Evaluate (SourceText line)

directiveCommand :: (Text, Text) -> Command
directiveCommand (name, argument)
  | name `elem` ["quit", "q"] = Quit
  | name `elem` ["help", "h", "?"] = ShowHelp
  | name `elem` ["type", "t"] = TypeOf (SourceText argument)
  | name == "history" = ShowHistory
  | otherwise = Unrecognized name

data Response
  = Answer !RenderedResult
  | Rejection !Int !SourceText !CalculatorError
  | Signature !Judgement
  | History !(Seq Entry)
  | Usage
  | UnknownCommand !Text
  deriving stock (Eq, Show)

data Transition
  = Continue !Session ![Response]
  | Halt
  deriving stock (Eq, Show)

step :: Session -> Text -> Transition
step session@(Session configuration history) line = case parseCommand line of
  Blank -> Continue session []
  Quit -> Halt
  ShowHelp -> Continue session [Usage]
  ShowHistory -> Continue session [History history]
  Unrecognized name -> Continue session [UnknownCommand name]
  TypeOf source -> Continue session [either (rejection source) Signature (judge source)]
  Evaluate source -> case calculate configuration source of
    Left failure -> Continue session [rejection source failure]
    Right result -> Continue (Session configuration (history |> Entry source result)) [Answer result]
  where
    rejection source@(SourceText text) = Rejection (Text.length line - Text.length text) source

data Interaction = Interactive | Batch
  deriving stock (Eq, Show)

prompt :: Text
prompt = "λ> "

present :: Interaction -> Response -> [Text]
present mode = \case
  Answer (RenderedResult result) -> [result]
  Rejection column source failure -> diagnose (anchor column) source failure
  Signature (Judgement syntax sort) -> [printSyntax syntax <> " : " <> sortSymbol sort]
  History entries -> zipWith historyLine [1 :: Int ..] (toList entries)
  Usage -> usage
  UnknownCommand name -> ["error: unknown command :" <> name <> "; type :help for a list of commands"]
  where
    anchor column = case mode of
      Interactive -> UnderEchoedInput (Text.length prompt + column)
      Batch -> UnderQuotedSource
    historyLine index (Entry (SourceText input) (RenderedResult result)) =
      Text.pack (show index) <> ". " <> Text.strip input <> " = " <> result

sortSymbol :: Sort -> Text
sortSymbol = \case
  N -> "ℕ"
  Z -> "ℤ"
  Q -> "ℚ"

usage :: [Text]
usage =
  [ "Enter an arithmetic expression, for example 1 + 2 * (3 - 4)."
  , "Operators: + - * / ^ and parentheses. Exponents must be integers."
  , ""
  , "  :type EXPRESSION   show the number sort of an expression (ℕ, ℤ or ℚ)"
  , "  :history           list the expressions evaluated so far"
  , "  :help              show this message"
  , "  :quit              leave the calculator (Ctrl-D also works)"
  ]

data Terminal m = Terminal
  { interaction :: !Interaction
  , receive :: m (Maybe Text)
  , emit :: Text -> m ()
  }

runRepl :: (Monad m) => Terminal m -> Session -> m ()
runRepl terminal = loop
  where
    loop session =
      receive terminal >>= \case
        Nothing -> pure ()
        Just line -> case step session line of
          Halt -> pure ()
          Continue next responses -> do
            traverse_ (emit terminal) (foldMap (present (interaction terminal)) responses)
            loop next
