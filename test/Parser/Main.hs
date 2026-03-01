{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Expression (..), Statement (..), expressionToString, prettyPrintStatement, statementToString, (?==))
import Control.Monad
import Debug.Trace
import Parser
import Test.Hspec
import Token (LexerState (..), Position (..), Token (..), TokenType (..), runLexer, tokenizer, tokenizerAll)

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

main :: IO ()
main = hspec spec

specOpPrecedence :: Spec
specOpPrecedence = do
  describe "prefix expression" $ do
    let testCases =
          [ ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("(3 + 3) * 3", "((3 + 3) * 3)")
          ]
    forM_ testCases $ \(input, expectedString) -> do
      describe ("parsing: " ++ input) $ do
        let state = initialState input
        case runAstParser parseProgram state of
          Left err ->
            it "should parse successfully" $
              expectationFailure ("Parser failed: " ++ show err)
          Right (program, _) ->
            it "should have the correct program string" $ do
              statementToString program `shouldBe` expectedString

spec :: Spec
spec = do
  describe "position tracking" $ do
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
            Right (Program stmts, _) ->
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
            Right (Program stmts, _) ->
              testReturnStatementSpec exprPredicate (head stmts)
    describe "prefix expression" $ do
      let testCases =
            [ ("!that", isBangWithExpr (IdentifierLit identityToken "that")),
              ("-10", isMinusWithExpr (IntLit identityToken 10)),
              ("!true", isBangWithExpr (BooleanLit identityToken True)),
              ("!false", isBangWithExpr (BooleanLit identityToken False)),
              ("!False", isBangWithExpr (IdentifierLit identityToken "False"))
            ]
      forM_ testCases $ \(input, exprPredicate) -> do
        describe ("parsing: " ++ input) $ do
          let state = initialState input
          case runAstParser parseProgram state of
            Left err ->
              it "should parse successfully" $
                expectationFailure ("Parser failed: " ++ show err)
            Right (Program stmts, _) ->
              testExpressionStatementSpec exprPredicate (head stmts)

literalTest :: Spec
literalTest = do
  describe "integer literal" $ do
    let state = initialState "5;"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        testExpressionStatementSpec (isIntLitWithValue 5) (head stmts)
  describe "string literal" $ do
    let state = initialState "\"that\""
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        testExpressionStatementSpec (isStringLitWith "that") (head stmts)
  describe "float literal" $ do
    let state = initialState "3.10"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        testExpressionStatementSpec (isFloatLitWith 3.10) (head stmts)
  describe "array literal" $ do
    let state = initialState "[10 ,  \"that\"]"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        it "should parse function literal correctly" $
          case head stmts of
            ExpressionStatement _ (ArrayLit _ elements) -> do
              elements
                `shouldSatisfy` ( \ps ->
                                    length ps == 2
                                      && and (zipWith (?==) ps [IntLit identityToken 10, StringLit identityToken "that"])
                                )
  describe "index expression parsing" $ do
    let state = initialState "myarray[1+1]"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        it "should parse index expression correctly" $
          case head stmts of
            ExpressionStatement _ expr@(IndexExpression _ left indexExpr) -> do
              expr `shouldSatisfy` (?== IndexExpression identityToken (IdentifierLit identityToken "myarray") (InfixExpression identityToken (IntLit identityToken 1) Plus (IntLit identityToken 1)))
            _ -> expectationFailure "Expected IndexExpression"

  describe "function literal" $ do
    let state = initialState "fn(x,y){5;}"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        it "should parse function literal correctly" $
          case head stmts of
            ExpressionStatement _ (FunctionLit _ parameters body) -> do
              parameters
                `shouldSatisfy` ( \ps ->
                                    length ps == 2
                                      && and (zipWith (?==) ps [IdentifierLit identityToken "x", IdentifierLit identityToken "y"])
                                )
              length body `shouldBe` 1
              case head body of
                ExpressionStatement _ expr ->
                  expr `shouldSatisfy` isIntLitWithValue 5
                _ -> expectationFailure "Expected ExpressionStatement in body"
            _ -> expectationFailure "Expected FunctionLit"

callExpr :: Spec
callExpr = do
  describe "parsing call expression" $ do
    let state = initialState "call(x,y);"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        it "should parse function literal correctly" $
          case head stmts of
            ExpressionStatement _ (CallExpression _ function args) ->
              case function of
                IdentifierLit _ name -> do
                  name `shouldBe` "call"
                  args
                    `shouldSatisfy` ( \ps ->
                                        length ps == 2
                                          && and (zipWith (?==) ps [IdentifierLit identityToken "x", IdentifierLit identityToken "y"])
                                    )
                _ -> expectationFailure "Expected IdentifierLit"
            _ -> expectationFailure "Expected CallExpression"
  describe "parsing call expression" $ do
    let state = initialState "fn(x,y){5;}(x,y);"
    case runAstParser parseProgram state of
      Left err ->
        it "should parse successfully" $
          expectationFailure ("Parser failed: " ++ show err)
      Right (Program stmts, _) ->
        it "should parse function literal correctly" $
          case head stmts of
            ExpressionStatement _ (CallExpression _ function args) ->
              case function of
                FunctionLit {} -> do
                  args
                    `shouldSatisfy` ( \ps ->
                                        length ps == 2
                                          && and (zipWith (?==) ps [IdentifierLit identityToken "x", IdentifierLit identityToken "y"])
                                    )
                _ -> expectationFailure "Expected FunctionLit"
            _ -> expectationFailure "Expected CallExpression"

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
      _ -> expectationFailure "not an expression statment"

testOpPrecendenceStringSpec :: String -> Statement -> Spec
testOpPrecendenceStringSpec expected stmt = do
  it "should be a ExpressionStatement" $
    stmt `shouldSatisfy` isExpressionStatement

  it "should have correct value expression" $
    case stmt of
      ExpressionStatement {expr = expr} ->
        expressionToString expr `shouldBe` expected
      _ -> expectationFailure "not an expression statment"

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
isIntLitWithValue :: Int -> Expression -> Bool
isIntLitWithValue expected (IntLit _ val) = val == expected
isIntLitWithValue _ _ = False

isStringLitWith :: String -> Expression -> Bool
isStringLitWith expected (StringLit _ val) = val == expected
isStringLitWith _ _ = False

isFloatLitWith :: Float -> Expression -> Bool
isFloatLitWith expected (FloatLit _ val) = val == expected
isFloatLitWith _ _ = False

isIdentifier :: String -> Expression -> Bool
isIdentifier expected (IdentifierLit _ name) = name == expected
isIdentifier _ _ = False

isStringLit :: String -> Expression -> Bool
isStringLit expected (StringLit _ val) = val == expected
isStringLit _ _ = False

isBoolLit :: Bool -> Expression -> Bool
isBoolLit expected (BooleanLit _ val) = val == expected
isBoolLit _ _ = False

isBangExpr :: Expression -> Bool
isBangExpr (PrefixExpression _ op _) = op == Bang
isBangExpr _ = False

isBangWithExpr :: Expression -> Expression -> Bool
isBangWithExpr expected (PrefixExpression _ Bang expr) = expr ?== expected
isBangWithExpr _ _ = False

isMinusExpr :: Expression -> Bool
isMinusExpr (PrefixExpression _ op _) = op == Minus
isMinusExpr _ = False

isMinusWithExpr :: Expression -> Expression -> Bool
isMinusWithExpr expected (PrefixExpression _ Minus expr) = expr ?== expected
isMinusWithExpr _ _ = False

identityToken :: Token
identityToken = Token Illegal (Position 0 0)

prettyTest :: String -> IO ()
prettyTest input =
  case runAstParser parseProgram (initialState input) of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right (prog, _) -> putStrLn $ prettyPrintStatement 0 prog
