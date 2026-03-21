{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Expression (..), Statement (..), expressionToString, prettyPrintStatement, statementToString, (?==))
import Control.Monad
import Control.Monad.State.Lazy
import Data.Map qualified as Map
import Debug.Trace
import Eval
import Parser
import Test.Hspec
import Test.QuickCheck.Property (Result (testCase))
import Token (LexerState (..), Position (..), Token (..), TokenType (..), runLexer, tokenizer, tokenizerAll)

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

main :: IO ()
main = hspec $ do
  testIntegerObj
  testBoolObj
  testBangOperator
  testIfElse
  testReturnStatement
  testClosures
  testFunctionApplication
  testIndex
  testStruct

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
        Right (prog, _) -> prettyPrintStatement 0 prog
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
          [ ("if (true) { return 10 }", 10),
            ("if (1) { return 10 }", 10),
            ("if (1 < 2) { return 10 }", 10),
            ("if (1 > 2) { return 10 } else { return 20 }", 20),
            ("if (1 < 2) { return 10 } else { return 20 }", 10)
          ]

    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj (fromInteger expectedValue)

  describe "nil cases" $ do
    let testCases :: [(String, Object)]
        testCases =
          [ ("if (1 > 2) { 10 }", VoidObj),
            ("if (false) { 10 }", VoidObj)
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
          evalHelper input `shouldBe` IntObj (fromInteger expectedValue)

testClosures :: Spec
testClosures = do
  describe "closures" $ do
    let testCases :: [(String, Integer)]
        testCases =
          [ ("let f = fn(x) { return fn(y) { return x + y } }; let adder = f(10); adder(5);", 15)
          ]
    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj (fromInteger expectedValue)

testFunctionApplication :: Spec
testFunctionApplication = do
  describe "Function application" $ do
    let testCases :: [(String, Integer)]
        testCases =
          [ ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { return x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { return x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { return x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { return x; }(5)", 5)
          ]
    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          evalHelper input `shouldBe` IntObj (fromInteger expectedValue)

testIndex :: Spec
testIndex = describe "Test indexing" $ do
  it "indexes an integer element" $
    evalHelper "[1, 2, 10, 4][0];" `shouldBe` IntObj 1
  it "indexes into the middle" $
    evalHelper "[1, 2, 10, 4][2];" `shouldBe` IntObj 10
  it "returns null on out of bounds" $
    evalHelper "[1, 2, 3][5];" `shouldBe` ErrorObj (IndexOutOfBounds {outOfBoundsIndex = 5, arrayLength = 3, pos = Just (Position {_line = 1, _column = 10})})
  it "returns error on non-integer index" $
    evalHelper "[1, 2, 3][true];"
      `shouldBe` ErrorObj
        InvalidIndexType
          { expected = "Integer",
            got = "Boolean",
            pos = Just Position {_line = 1, _column = 10}
          }

testStruct :: Spec
testStruct = describe "Test struct initialization and field access" $ do
  -- Basic struct initialization
  it "creates a simple struct with integer fields" $
    evalHelper "Point { x: 10; y: 20 };" `shouldBe` StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])

  it "creates a struct with mixed field types" $
    evalHelper "Person { name: \"Alice\"; age: 30; active: true };"
      `shouldBe` StructObj "Person" (Map.fromList [("name", StringObj "Alice"), ("age", IntObj 30), ("active", BoolObj True)])

  -- Struct in let binding
  it "binds struct to variable" $ do
    let result = evalHelper "let p = Point { x: 10; y: 20 }; p;"
    result `shouldBe` StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])

  -- Field access
  it "accesses struct field" $
    evalHelper "let p = Point { x: 10; y: 20 }; p.x;" `shouldBe` IntObj 10

  it "accesses different struct fields" $
    evalHelper "let p = Point { x: 10; y: 20 }; p.y;" `shouldBe` IntObj 20

  it "accesses string field from struct" $
    evalHelper "let person = Person { name: \"Bob\"; age: 25 }; person.name;"
      `shouldBe` StringObj "Bob"

  -- Struct with expressions as values
  it "evaluates expressions in field values" $
    evalHelper "Point { x: 5 + 5; y: 10 * 2 };"
      `shouldBe` StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])

  it "calls function in field value" $
    evalHelper "let add = fn(a, b) { return a + b }; Point { x: add(5, 5); y: 20 };"
      `shouldBe` StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])

  -- Nested structs
  it "creates nested struct" $ do
    let inner1 = StructObj "Point" (Map.fromList [("x", IntObj 0), ("y", IntObj 0)])
    let inner2 = StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 10)])
    evalHelper "Line { start: Point { x: 0; y: 0 }; end: Point { x: 10; y: 10 } };"
      `shouldBe` StructObj "Line" (Map.fromList [("start", inner1), ("end", inner2)])

  it "accesses nested struct field" $
    evalHelper "let line = Line { start: Point { x: 0; y: 0 }; end: Point { x: 10; y: 10 } }; line.start.x;"
      `shouldBe` IntObj 0

  it "accesses deeply nested field" $
    evalHelper "let line = Line { start: Point { x: 5; y: 10 }; end: Point { x: 15; y: 20 } }; line.end.y;"
      `shouldBe` IntObj 20

  -- Struct declarations (should be ignored in eval mode)
  it "ignores struct declaration" $
    evalHelper "Struct Point { x: Int; y: Int }; return 42;" `shouldBe` IntObj 42

  it "creates struct even without declaration" $
    evalHelper "AnyStructName { field1: 1; field2: 2 };"
      `shouldBe` StructObj "AnyStructName" (Map.fromList [("field1", IntObj 1), ("field2", IntObj 2)])

  -- Error cases
  it "returns error on field access of non-struct" $
    evalHelper "let x = 10; x.foo;"
      `shouldSatisfy` ( \case
                          ErrorObj (InvalidFieldAcess _ _ _) -> True
                          _ -> False
                      )

  it "returns error on accessing non-existent field" $
    evalHelper "let p = Point { x: 10; y: 20 }; p.z;"
      `shouldSatisfy` \case
        ErrorObj (InvalidFieldAcess _ _ _) -> True
        _ -> False

  -- Struct in array
  it "stores struct in array" $ do
    let p1 = StructObj "Point" (Map.fromList [("x", IntObj 1), ("y", IntObj 2)])
    let p2 = StructObj "Point" (Map.fromList [("x", IntObj 3), ("y", IntObj 4)])
    evalHelper "[Point { x: 1; y: 2 }, Point { x: 3; y: 4 }];"
      `shouldBe` ArrayObj [p1, p2]

  it "accesses struct field from array element" $
    evalHelper "let points = [Point { x: 1; y: 2 }, Point { x: 3; y: 4 }]; points[0].x;"
      `shouldBe` IntObj 1

  -- Struct returned from function
  it "returns struct from function" $
    evalHelper "let makePoint = fn(x, y) { return Point { x: x; y: y } }; makePoint(5, 10);"
      `shouldBe` StructObj "Point" (Map.fromList [("x", IntObj 5), ("y", IntObj 10)])

  it "accesses field of struct returned from function" $
    evalHelper "let makePoint = fn(x, y) { return Point { x: x; y: y } }; makePoint(7, 14).y;"
      `shouldBe` IntObj 14
