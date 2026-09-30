{-# LANGUAGE OverloadedStrings #-}

module Calculator.ReplSpec
  ( spec
  )
where

import Calculator.Application (defaultConfiguration)
import Calculator.Render (RenderedResult (..))
import Calculator.Repl
  ( Command (..)
  , Interaction (..)
  , Response (..)
  , Terminal (..)
  , Transition (..)
  , openSession
  , parseCommand
  , present
  , runRepl
  , step
  )
import Calculator.Syntax (SourceText (..))
import Control.Monad.Trans.State.Strict (State, execState, modify', state)
import Data.Foldable (for_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parseCommand" $
    for_ commands $ \(line, expected) ->
      it (show line) $ parseCommand line `shouldBe` expected
  describe "step" $ do
    it "answers an expression" $
      responsesOf "2 + 2" `shouldBe` [Answer (RenderedResult "4")]
    it "records answered expressions in the history" $
      responsesTo ["2 + 2", ":history"] `shouldBe` [["4"], ["1. 2 + 2 = 4"]]
    it "does not record a failed expression" $
      responsesTo ["1 / 0", ":history"] `shouldBe` [["error: division by zero"], []]
    it "halts on :quit" $
      step (openSession defaultConfiguration) ":quit" `shouldBe` Halt
    it "ignores blank lines" $
      step (openSession defaultConfiguration) "   "
        `shouldBe` Continue (openSession defaultConfiguration) []
  describe "present" $ do
    it "aligns the caret with input typed after the prompt" $
      presented Interactive "1 + * 2"
        `shouldBe` ["       ^", "error: unexpected '*'; expecting '(', '-', or number"]
    it "aligns the caret with the argument of :type" $
      presented Interactive ":type 1 +"
        `shouldBe` ["            ^", "error: unexpected end of input; expecting '(', '-', or number"]
    it "prints a typing judgement" $
      presented Batch ":t 2 ^ 10" `shouldBe` ["2 ^ 10 : ℕ"]
  describe "runRepl" $ do
    it "runs a session until :quit" $
      session ["2 + 2", "(21 * 2) + 27", "1 / 0", ":history", ":quit", "5 + 5"]
        `shouldBe` ["4", "69", "error: division by zero", "1. 2 + 2 = 4", "2. (21 * 2) + 27 = 69"]
    it "stops at the end of input" $
      session ["2 ^ 10"] `shouldBe` ["1024"]
    it "reports unknown commands and carries on" $
      session [":frobnicate", "1 + 1"]
        `shouldBe` ["error: unknown command :frobnicate; type :help for a list of commands", "2"]

commands :: [(Text, Command)]
commands =
  [ ("2 + 2", Evaluate (SourceText "2 + 2"))
  , ("  ", Blank)
  , (":", Blank)
  , (":q", Quit)
  , (":quit", Quit)
  , (":help", ShowHelp)
  , (":?", ShowHelp)
  , (":history", ShowHistory)
  , (":t 1 + 2", TypeOf (SourceText " 1 + 2"))
  , (":type 1", TypeOf (SourceText " 1"))
  , (":nope", Unrecognized "nope")
  ]

responsesOf :: Text -> [Response]
responsesOf line = case step (openSession defaultConfiguration) line of
  Continue _ responses -> responses
  Halt -> []

responsesTo :: [Text] -> [[Text]]
responsesTo = go (openSession defaultConfiguration)
  where
    go current = \case
      [] -> []
      line : rest -> case step current line of
        Halt -> []
        Continue next responses -> foldMap (present Batch) responses : go next rest

presented :: Interaction -> Text -> [Text]
presented mode = foldMap (present mode) . responsesOf

session :: [Text] -> [Text]
session inputs = reverse (snd (execState (runRepl scripted (openSession defaultConfiguration)) (inputs, [])))

scripted :: Terminal (State ([Text], [Text]))
scripted =
  Terminal
    { interaction = Batch
    , receive = state $ \case
        ([], emitted) -> (Nothing, ([], emitted))
        (line : rest, emitted) -> (Just line, (rest, emitted))
    , emit = \text -> modify' (fmap (text :))
    }
