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
import Test.Hspec.Expectations (Expectation, expectationFailure)
import Test.QuickCheck.Property (Result (testCase))
import Token (LexerState (..), Position (..), Token (..), TokenType (..), runLexer, tokenizer, tokenizerAll)

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1, _absPos = 0}}

infix 1 `shouldBeMsg`

shouldBeMsg :: (Eq a, Show a) => a -> a -> String -> Expectation
shouldBeMsg actual expected msg =
  if actual == expected
    then pure ()
    else
      expectationFailure $
        msg ++ "\nExpected: " ++ show expected ++ "\nBut got: " ++ show actual

main :: IO ()
main = hspec $ do
  testIntegerObj
  testBoolObj
  testBangOperator
  testIfElse
  testReturnStatement
  testClosures
  testAssigment
  testFunctionApplication
  testIndex
  testStruct
  testLogicalOperators

evalHelper :: String -> (Object, String)
evalHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog, _) -> (fst $ runState (evalProgram prog) newEnv, "No error")
        Left parseErr ->
          let evalerr = (astParserErrorToEvalError parseErr) in (ErrorObj evalerr, toSourceCode program evalerr)

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` IntObj expectedValue $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` BoolObj expectedValue $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` BoolObj expectedValue $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` IntObj (fromInteger expectedValue) $ err

  describe "nil cases" $ do
    let testCases :: [(String, Object)]
        testCases =
          [ ("if (1 > 2) { 10 }", VoidObj),
            ("if (false) { 10 }", VoidObj)
          ]

    forM_ testCases $ \(input, expectedValue) -> do
      describe ("parsing: " ++ input) $ do
        it ("It should have value " ++ show expectedValue) $ do
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` (expectedValue) $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` IntObj (fromInteger expectedValue) $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` IntObj (fromInteger expectedValue) $ err

testAssigment :: Spec
testAssigment = do
  describe "AssignmentStatements" $ do
    it "should return value" $ do
      let input = "let factorial = fn(n) {\n  if (n < 2) {\n    return 1;\n  }\n  return n * factorial(n - 1);\n};\nfactorial(5);\nlet y = 0;\ny = factorial(10);"
          expectedValue = 3628800
      let (obj, err) = evalHelper input
      obj `shouldBeMsg` (IntObj (fromInteger expectedValue)) $ err
    it "should fail since it hasn't been assinged" $ do
      let input = "let factorial = fn(n) {\n  if (n < 2) {\n    return 1;\n  }\n  return n * factorial(n - 1);\n};\nfactorial(5);\n\ny = factorial(10);"
      let (obj, err) = evalHelper input
      obj `shouldBeMsg` (ErrorObj (FaildAssigment {varName = "y", pos = Nothing})) $ err

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
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` (IntObj (fromInteger expectedValue)) $ err

testIndex :: Spec
testIndex = describe "Test indexing" $ do
  it "indexes an integer element" $ do
    let (obj, err) = evalHelper "[1, 2, 10, 4][0];"
    obj `shouldBeMsg` (IntObj 1) $ err
  it "indexes into the middle" $ do
    let (obj, err) = evalHelper "[1, 2, 10, 4][2];"
    obj `shouldBeMsg` (IntObj 10) $ err
  it "returns null on out of bounds" $ do
    let (obj, err) = evalHelper "[1, 2, 3][5];"
    obj `shouldBeMsg` (ErrorObj (IndexOutOfBounds {outOfBoundsIndex = 5, arrayLength = 3, pos = Just (Position {_line = 1, _column = 10, _absPos = 9})})) $ err
  it "returns error on non-integer index" $ do
    let (obj, err) = evalHelper "[1, 2, 3][true];"
    obj `shouldBeMsg` (ErrorObj (InvalidIndexType {expected = "Integer", got = "Boolean", pos = Just Position {_line = 1, _column = 10, _absPos = 9}})) $ err

testStruct :: Spec
testStruct = describe "Test struct initialization and field access" $ do
  -- Basic struct initialization
  it "creates a simple struct with integer fields" $ do
    let (obj, err) = evalHelper "Point { x: 10; y: 20 };"
    obj `shouldBeMsg` (StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])) $ err

  it "creates a struct with mixed field types" $ do
    let (obj, err) = evalHelper "Person { name: \"Alice\"; age: 30; active: true };"
    obj `shouldBeMsg` (StructObj "Person" (Map.fromList [("name", StringObj "Alice"), ("age", IntObj 30), ("active", BoolObj True)])) $ err

  -- Struct in let binding
  it "binds struct to variable" $ do
    let (obj, err) = evalHelper "let p = Point { x: 10; y: 20 }; p;"
    obj `shouldBeMsg` (StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])) $ err

  -- Field access
  it "accesses struct field" $ do
    let (obj, err) = evalHelper "let p = Point { x: 10; y: 20 }; p.x;"
    obj `shouldBeMsg` (IntObj 10) $ err

  it "accesses different struct fields" $ do
    let (obj, err) = evalHelper "let p = Point { x: 10; y: 20 }; p.y;"
    obj `shouldBeMsg` (IntObj 20) $ err

  it "accesses string field from struct" $ do
    let (obj, err) = evalHelper "let person = Person { name: \"Bob\"; age: 25 }; person.name;"
    obj `shouldBeMsg` (StringObj "Bob") $ err

  -- Struct with expressions as values
  it "evaluates expressions in field values" $ do
    let (obj, err) = evalHelper "Point { x: 5 + 5; y: 10 * 2 };"
    obj `shouldBeMsg` (StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])) $ err

  it "calls function in field value" $ do
    let (obj, err) = evalHelper "let add = fn(a, b) { return a + b }; Point { x: add(5, 5); y: 20 };"
    obj `shouldBeMsg` (StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 20)])) $ err

  -- Nested structs
  it "creates nested struct" $ do
    let inner1 = StructObj "Point" (Map.fromList [("x", IntObj 0), ("y", IntObj 0)])
    let inner2 = StructObj "Point" (Map.fromList [("x", IntObj 10), ("y", IntObj 10)])
    let (obj, err) = evalHelper "Line { start: Point { x: 0; y: 0 }; end: Point { x: 10; y: 10 } };"
    obj `shouldBeMsg` (StructObj "Line" (Map.fromList [("start", inner1), ("end", inner2)])) $ err

  it "accesses nested struct field" $ do
    let (obj, err) = evalHelper "let line = Line { start: Point { x: 0; y: 0 }; end: Point { x: 10; y: 10 } }; line.start.x;"
    obj `shouldBeMsg` (IntObj 0) $ err

  it "accesses deeply nested field" $ do
    let (obj, err) = evalHelper "let line = Line { start: Point { x: 5; y: 10 }; end: Point { x: 15; y: 20 } }; line.end.y;"
    obj `shouldBeMsg` (IntObj 20) $ err

  -- Struct declarations (should be ignored in eval mode)
  it "ignores struct declaration" $ do
    let (obj, err) = evalHelper "Struct Point { x: Int; y: Int }; return 42;"
    obj `shouldBeMsg` (IntObj 42) $ err

  it "creates struct even without declaration" $ do
    let (obj, err) = evalHelper "AnyStructName { field1: 1; field2: 2 };"
    obj `shouldBeMsg` (StructObj "AnyStructName" (Map.fromList [("field1", IntObj 1), ("field2", IntObj 2)])) $ err

  -- Error cases
  it "returns error on field access of non-struct" $ do
    let (obj, _) = evalHelper "let x = 10; x.foo;"
    obj `shouldSatisfy` \case
      ErrorObj (InvalidFieldAcess _ _ _) -> True
      _ -> False

  it "returns error on accessing non-existent field" $ do
    let (obj, _) = evalHelper "let p = Point { x: 10; y: 20 }; p.z;"
    obj `shouldSatisfy` \case
      ErrorObj (InvalidFieldAcess _ _ _) -> True
      _ -> False

  -- Struct in array
  it "stores struct in array" $ do
    let p1 = StructObj "Point" (Map.fromList [("x", IntObj 1), ("y", IntObj 2)])
    let p2 = StructObj "Point" (Map.fromList [("x", IntObj 3), ("y", IntObj 4)])
    let (obj, err) = evalHelper "[Point { x: 1; y: 2 }, Point { x: 3; y: 4 }];"
    obj `shouldBeMsg` (ArrayObj [p1, p2]) $ err

  it "accesses struct field from array element" $ do
    let (obj, err) = evalHelper "let points = [Point { x: 1; y: 2 }, Point { x: 3; y: 4 }]; points[0].x;"
    obj `shouldBeMsg` (IntObj 1) $ err

  -- Struct returned from function
  it "returns struct from function" $ do
    let (obj, err) = evalHelper "let makePoint = fn(x, y) { return Point { x: x; y: y } }; makePoint(5, 10);"
    obj `shouldBeMsg` (StructObj "Point" (Map.fromList [("x", IntObj 5), ("y", IntObj 10)])) $ err

  it "accesses field of struct returned from function" $ do
    let (obj, err) = evalHelper "let makePoint = fn(x, y) { return Point { x: x; y: y } }; makePoint(7, 14).y;"
    obj `shouldBeMsg` (IntObj 14) $ err

testLogicalOperators :: Spec
testLogicalOperators = do
  describe "logical && and || operators" $ do
    let testCases :: [(String, Object)]
        testCases =
          [ ("true && true", BoolObj True),
            ("true && false", BoolObj False),
            ("false && true", BoolObj False),
            ("false && false", BoolObj False),
            ("true || true", BoolObj True),
            ("true || false", BoolObj True),
            ("false || true", BoolObj True),
            ("false || false", BoolObj False),
            ("1 && 1", BoolObj True),
            ("1 && 0", BoolObj False),
            ("0 && 1", BoolObj False),
            ("1 || 0", BoolObj True),
            ("0 || 1", BoolObj True),
            ("0 || 0", BoolObj False),
            ("\"hello\" && \"world\"", BoolObj True),
            ("\"\" && \"world\"", BoolObj False),
            ("\"hello\" || \"world\"", BoolObj True),
            ("\"\" || \"world\"", BoolObj True),
            ("(1 < 2) && (3 > 2)", BoolObj True),
            ("(1 < 2) && (3 < 2)", BoolObj False),
            ("(1 > 2) || (3 > 2)", BoolObj True)
          ]
    forM_ testCases $ \(input, expected) -> do
      describe ("parsing: " ++ input) $ do
        it ("should evaluate to " ++ show expected) $ do
          let (obj, err) = evalHelper input
          obj `shouldBeMsg` expected $ err

    it "short-circuits && when left is falsy" $ do
      let program = "let f = fn(x) { return x }; false && f(1);"
          (obj, err) = evalHelper program
      obj `shouldBeMsg` (BoolObj False) $ err

    it "short-circuits || when left is truthy" $ do
      let program = "let f = fn(x) { return x }; true || f(1);"
          (obj, err) = evalHelper program
      obj `shouldBeMsg` (BoolObj True) $ err

    it "evaluates right side of && when left is truthy" $ do
      let (obj, err) = evalHelper "true && 42"
      obj `shouldBeMsg` (BoolObj True) $ err

    it "evaluates right side of || when left is falsy" $ do
      let (obj, err) = evalHelper "false || 42"
      obj `shouldBeMsg` (BoolObj True) $ err
