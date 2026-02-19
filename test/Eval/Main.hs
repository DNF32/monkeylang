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
import Test.QuickCheck.Property (Result (testCase))
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

parserHelper :: String -> String
parserHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog, _) -> show prog
        Left parseErr -> show parseErr

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

testBoolObj :: Spec
testBoolObj = do
  describe "bool objects" $ do
    let testCases =
          [ ("true", True),
            ("false", False),
            ("1 < 2", True),
            ("1 > 2", False),
            ("1 < 1", False),
            ("1 > 1", False),
            ("1 == 1", True),
            ("1 != 1", False),
            ("1 == 2", False),
            ("1 != 2", True),
            ("true == true", True),
            ("false == false", True),
            ("true == false", False),
            ("true != false", True),
            ("false != true", True),
            ("(1 < 2) == true", True),
            ("(1 < 2) == false", False),
            ("(1 > 2) == true", False),
            ("(1 > 2) == false", True)
          ]
    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value" ++ show expectedValue) $ do
          evalHelper input `shouldBe` BoolObj expectedValue

testBangOperator :: Spec
testBangOperator = do
  describe "bool objects" $ do
    let testCases =
          [ ("!true", False),
            ("!false", True),
            ("!5", False),
            ("!!true", True),
            ("!!false", False),
            ("!!5", True)
          ]
    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value" ++ show expectedValue) $ do
          evalHelper input `shouldBe` BoolObj expectedValue

testIfElse :: Spec
testIfElse = do
  describe "bool objects" $ do
    let testCases :: [(String, Integer)]
        testCases =
          [ ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10)
          ]

    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj expectedValue

  describe "nil cases" $ do
    let testCases :: [(String, Object)]
        testCases =
          [ ("if (1 > 2) { 10 }", NullObj)
          ]

    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` expectedValue

testReturnStatement :: Spec
testReturnStatement = do
  describe "bool objects" $ do
    let testCases :: [(String, Integer)]
        testCases =
          [ ("if (10 > 1) { return 10; }", 10),
            ("if (10 > 1) { if (10 > 1) { return 10;} return 1;}", 10),
            ("let f = fn(x) {\n  return x;\n  x + 10;\n};\nf(10);", 10),
            ("let f = fn(x) {\n   let result = x + 10;\n   return result;\n   return 10;\n};\nf(10);", 20)
          ]

    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj expectedValue
