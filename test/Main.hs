module Main
  ( main
  )
where

import Calculator.AlgebraSpec qualified as AlgebraSpec
import Calculator.ApplicationSpec qualified as ApplicationSpec
import Calculator.InterpreterSpec qualified as InterpreterSpec
import Calculator.NumberSpec qualified as NumberSpec
import Calculator.ParserSpec qualified as ParserSpec
import Calculator.RenderSpec qualified as RenderSpec
import Calculator.ReplSpec qualified as ReplSpec
import Calculator.ValidationSpec qualified as ValidationSpec
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Calculator.Algebra" AlgebraSpec.spec
  describe "Calculator.Number" NumberSpec.spec
  describe "Calculator.Parser" ParserSpec.spec
  describe "Calculator.Validation" ValidationSpec.spec
  describe "Calculator.Interpreter" InterpreterSpec.spec
  describe "Calculator.Render" RenderSpec.spec
  describe "Calculator.Application" ApplicationSpec.spec
  describe "Calculator.Repl" ReplSpec.spec
