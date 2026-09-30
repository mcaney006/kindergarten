{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Calculator.Application
  ( Configuration (..)
  , calculate
  , defaultConfiguration
  , elaborate
  )
import Calculator.Expression (Program)
import Calculator.Interpreter (evaluate)
import Calculator.Parser (parseSyntax)
import Calculator.Syntax (SourceText (..))
import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (die)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "parse"
        [ bench "literal" (nf parseSyntax literal)
        , bench "nested" (nf parseSyntax nested)
        ]
    , bgroup
        "evaluate"
        [ env (programOf small) (bench "small" . nf (evaluate bound))
        , env (programOf large) (bench "large" . nf (evaluate bound))
        ]
    , bgroup
        "pipeline"
        [ bench "small" (nf (calculate defaultConfiguration) small)
        , bench "large" (nf (calculate defaultConfiguration) large)
        ]
    ]
  where
    bound = magnitudeBound defaultConfiguration

programOf :: SourceText -> IO Program
programOf source = either (die . show) (pure . snd) (elaborate source)

literal :: SourceText
literal = SourceText "3.14159265358979"

nested :: SourceText
nested = SourceText (foldr wrap "1" [1 .. 64 :: Int])
  where
    wrap depth inner = "(" <> integer depth <> " * " <> inner <> " - 1)"

small :: SourceText
small = SourceText "(8 * 4) / 2"

large :: SourceText
large = SourceText (Text.intercalate " + " (map term [1 .. 1_000 :: Int]))
  where
    term k = integer k <> " ^ 2 / (" <> integer k <> " + 1) - 0.5"

integer :: Int -> Text
integer = Text.pack . show
