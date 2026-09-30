module Calculator.CommandLine
  ( main
  )
where

import Calculator.Application
  ( Configuration (..)
  , calculate
  , defaultConfiguration
  )
import Calculator.Diagnostic (CaretAnchor (..), diagnose)
import Calculator.Render (Notation (..), Precision (..), RenderedResult (..))
import Calculator.Repl
  ( Interaction (..)
  , Terminal (..)
  , openSession
  , prompt
  , runRepl
  )
import Calculator.Syntax (SourceText (..))
import Control.Monad (when)
import Data.ByteString.Char8 qualified as Bytes
import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.IO qualified as Text
import Options.Applicative
  ( Parser
  , ParserInfo
  , auto
  , execParser
  , flag
  , forwardOptions
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , many
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , strArgument
  , value
  , (<**>)
  )
import System.Exit (exitFailure)
import System.IO (hFlush, hIsTerminalDevice, hSetEncoding, isEOF, stderr, stdin, stdout, utf8)

data Invocation = Invocation !Configuration !(Maybe SourceText)

main :: IO ()
main = do
  traverse_ (`hSetEncoding` utf8) [stdout, stderr]
  Invocation configuration expression <- execParser invocation
  maybe (converse configuration) (answer configuration) expression

answer :: Configuration -> SourceText -> IO ()
answer configuration source = case calculate configuration source of
  Right (RenderedResult result) -> Text.putStrLn result
  Left failure -> do
    traverse_ (Text.hPutStrLn stderr) (diagnose UnderQuotedSource source failure)
    exitFailure

converse :: Configuration -> IO ()
converse configuration = do
  attached <- hIsTerminalDevice stdin
  runRepl (terminal (if attached then Interactive else Batch)) (openSession configuration)

terminal :: Interaction -> Terminal IO
terminal mode =
  Terminal
    { interaction = mode
    , receive = do
        when interactive (Text.putStr prompt *> hFlush stdout)
        exhausted <- isEOF
        if exhausted
          then Nothing <$ when interactive (Text.putStrLn mempty)
          else Just . decodeUtf8Lenient <$> Bytes.hGetLine stdin
    , emit = Text.putStrLn
    }
  where
    interactive = mode == Interactive

invocation :: ParserInfo Invocation
invocation =
  info
    (arguments <**> helper)
    ( fullDesc
        <> forwardOptions
        <> header "calculator - exact arithmetic on the command line"
        <> progDesc
          "Evaluate EXPRESSION and print the result. \
          \Without an EXPRESSION, read expressions interactively."
    )

arguments :: Parser Invocation
arguments = Invocation <$> configurationOptions <*> expressionArgument

configurationOptions :: Parser Configuration
configurationOptions =
  Configuration
    <$> flag
      DecimalNotation
      FractionNotation
      (long "fraction" <> short 'f' <> help "Print results as exact fractions")
    <*> option
      (Precision <$> auto)
      ( long "precision"
          <> short 'p'
          <> metavar "DIGITS"
          <> value (precision defaultConfiguration)
          <> showDefault
          <> help "Maximum number of fractional digits to print"
      )
    <*> pure (magnitudeBound defaultConfiguration)

expressionArgument :: Parser (Maybe SourceText)
expressionArgument = assemble <$> many (strArgument (metavar "EXPRESSION"))
  where
    assemble = \case
      [] -> Nothing
      pieces -> Just (SourceText (Text.unwords pieces))
