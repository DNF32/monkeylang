{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Expression (..), Statement (..))
import Control.Monad
import Parser
import Test.Hspec
import Token (LexerState (..), Position (..), Token (..), TokenType (..))

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "position tracking" $ do
    it "parses let statement with correct components" $ do
      let state = initialState "let x = 10;"
      case runAstParser parseProgram state of
        Left err ->
          expectationFailure ("Parser failed: " ++ show err)
        Right (stmts, _) -> do
          length stmts `shouldBe` 1
          case head stmts of
            LetStatement
              { name = IdentifierLit {name = varName},
                value = IntLit {intValue = varVal}
              } -> do
                varName `shouldBe` "x"
                varVal `shouldBe` 10
            _ ->
              expectationFailure "Expected LetStatement with identifier and int literal"

    it "parses two statements" $ do
      let state = initialState "let x = 10; return 10"
      case runAstParser parseProgram state of
        Left err ->
          expectationFailure ("Parser failed: " ++ show err)
        Right (stmts, _) -> do
          length stmts `shouldBe` 2

    describe "let statement" $ do
      let testCases =
            [ ("let x = 10;", "x", isIntLitWithValue 10),
              ("let y = 20;", "y", isIntLitWithValue 20),
              ("let foo = 5;", "foo", isIntLitWithValue 5),
              ("let bar = 100;", "bar", isIntLitWithValue 100)
            ]

      forM_ testCases $ \(input, expectedId, exprPredicate) -> do
        describe ("parsing: " ++ input) $ do
          let state = initialState input
          case runAstParser parseProgram state of
            Left err ->
              it "should parse successfully" $
                expectationFailure ("Parser failed: " ++ show err)
            Right (stmts, _) ->
              testLetStatementSpec expectedId exprPredicate (head stmts)

    describe "return statement" $ do
      let testCases =
            [ ("return \"that is the end\";", isStringLit "that is the end"),
              ("return \"10\";", isStringLit "10"),
              ("return 10;", isIntLitWithValue 10)
            ]
      forM_ testCases $ \(input, exprPredicate) -> do
        describe ("parsing: " ++ input) $ do
          let state = initialState input
          case runAstParser parseProgram state of
            Left err ->
              it "should parse successfully" $
                expectationFailure ("Parser failed: " ++ show err)
            Right (stmts, _) ->
              testReturnStatementSpec exprPredicate (head stmts)

-- Helper that returns a Spec for detailed test output
testLetStatementSpec :: String -> (Expression -> Bool) -> Statement -> Spec
testLetStatementSpec expectedId exprPredicate parsedStatement = do
  it "should be a LetStatement" $
    parsedStatement `shouldSatisfy` isLetStatement

  it ("should have identifier name: " ++ expectedId) $
    case parsedStatement of
      LetStatement {name = IdentifierLit {name = varName}} ->
        varName `shouldBe` expectedId
      _ -> expectationFailure "Not a LetStatement"

  it "should have correct value expression" $
    case parsedStatement of
      LetStatement {value = expr} ->
        expr `shouldSatisfy` exprPredicate
      _ -> expectationFailure "Not a LetStatement"

testReturnStatementSpec :: (Expression -> Bool) -> Statement -> Spec
testReturnStatementSpec exprPredicate parsedStatement = do
  it "should be a ReturnStatement" $
    parsedStatement `shouldSatisfy` isReturnStatement

  it "should have correct value expression" $
    case parsedStatement of
      ReturnStatement {result = expr} ->
        expr `shouldSatisfy` exprPredicate
      _ -> expectationFailure "Not a ReturnStatement"

testExpressionStatementSpec :: (Expression -> Bool) -> Statement -> Spec
testExpressionStatementSpec exprPredicate parsedStatement = do
  it "should be a ExpressionStatement" $
    parsedStatement `shouldSatisfy` isExpressionStatement

  it "should have correct value expression" $
    case parsedStatement of
      ExpressionStatement {expr = expr} ->
        expr `shouldSatisfy` exprPredicate
      _ -> expectationFailure "Not a ReturnStatement"

-- Helper to check if statement is a LetStatement
isLetStatement :: Statement -> Bool
isLetStatement (LetStatement {}) = True
isLetStatement _ = False

isReturnStatement :: Statement -> Bool
isReturnStatement (ReturnStatement {}) = True
isReturnStatement _ = False

isExpressionStatement :: Statement -> Bool
isExpressionStatement (ExpressionStatement {}) = True
isExpressionStatement _ = False

-- Helper predicates for expressions
isIntLitWithValue :: Integer -> Expression -> Bool
isIntLitWithValue expected (IntLit _ val) = val == expected
isIntLitWithValue _ _ = False

isIdentifier :: String -> Expression -> Bool
isIdentifier expected (IdentifierLit _ name) = name == expected
isIdentifier _ _ = False

isStringLit :: String -> Expression -> Bool
isStringLit expected (StringLit _ val) = val == expected
isStringLit _ _ = False

isBoolLit :: Bool -> Expression -> Bool
isBoolLit expected (BooleanLit _ val) = val == expected
isBoolLit _ _ = False
