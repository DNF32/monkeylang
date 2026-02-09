{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Expression (..), Statement (..), expressionToString, prettyPrintStatement, statementToString, (?==))
import Control.Monad
import Control.Monad.State.Lazy
import Debug.Trace
import Eval
import Parser
import Test.Hspec
import Token (LexerState (..), Position (..), Token (..), TokenType (..), runLexer, tokenizer, tokenizerAll)

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

main :: IO ()
main = hspec testIntegerObj

evalHelper :: String -> Object
evalHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog, _) -> fst $ runState (evalProgram prog) newEnv
        Left parseErr -> ErrorObj (astParserErrorToEvalError parseErr)

testIntegerObj :: Spec
testIntegerObj = do
  describe "integer objects" $ do
    let testCases =
          [ ("5;", 5),
            ("5+5;", 10),
            ("5+5*5;", 30)
          ]
    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value" ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj expectedValue
