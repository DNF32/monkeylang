{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Statement (..), TStatement (..), prettyPrintStatement)
import Control.Monad.State.Lazy
import Parser
import Test.Hspec
import Token (LexerState (..), Position (..))
import TypeChecker
import Types

main :: IO ()
main = hspec $ do
  testLiterals
  testPrefixExpressions
  testInfixExpressions
  testLetStatements
  testFunctionTypeChecking

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

typeCheckerHelper :: String -> Either TypeError TStatement
typeCheckerHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog@(Program _), _) ->
          fst <$> runStateT (statementTypeChecker prog) emptyEnv
        Left parseErr ->
          Left $ InternalError {message = show parseErr, pos = Nothing}

parserHelper :: String -> String
parserHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog, _) -> prettyPrintStatement 0 prog
        Left parseErr -> show parseErr

testFunctionTypeChecking :: Spec
testFunctionTypeChecking = do
  describe "Funtion with type hint" $ do
    it "Should be function that takes in int and return string" $ do
      let program = "let x = fn(x:Int): String{return \"that\";}"
      case typeCheckerHelper program of
        Right (TProgram stmts) -> case head stmts of
          TLetStatement _ name value ty -> ty `shouldBe` (FnT [IntT] StringT)
  describe "Funtion with type hint" $ do
    it "Should be function that takes in int and return string" $ do
      let program = "let x = fn(x:Int): Int{return \"that\";}"
      case typeCheckerHelper program of
        Left err ->
          err
            `shouldBe` TypeMismatch
              { expected = IntT,
                got = StringT,
                pos =
                  Just
                    Position
                      { _line = 1,
                        _column = 9
                      }
              }
        Right _ -> expectationFailure "Expected TypeMismatch but typechecked successfully"
  describe "Funtion with type hint" $ do
    it "Should be function that takes in int and return string" $ do
      let program = "let x = fn(x:Int){return \"that\";}"
      case typeCheckerHelper program of
        Right (TProgram stmts) -> case head stmts of
          TLetStatement _ name value ty -> ty `shouldBe` (FnT [IntT] StringT)
        Left _ -> expectationFailure "Expected typechecked  but TypeMismatch  successfully"
    it "Should infer UnionT when if branches return different types" $ do
      let program = "let x = fn() { if (true) { return 5; } return \"hello\"; }"
      case typeCheckerHelper program of
        Right (TProgram stmts) -> case head stmts of
          TLetStatement _ _ _ ty -> ty `shouldBe` FnT [] (UnionT [StringT, IntT])
          _ -> expectationFailure "Expected TLetStatement"
        Left err -> expectationFailure (show err)

-- helpers

getLetType :: TStatement -> Maybe Types.Type
getLetType (TProgram (TLetStatement _ _ _ ty : _)) = Just ty
getLetType _ = Nothing

testLiterals :: Spec
testLiterals = describe "Literals" $ do
  it "integer literal gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = 5;" `shouldBe` Right (Just IntT)
  it "float literal gets FloatT" $ do
    getLetType <$> typeCheckerHelper "let x = 3.14;" `shouldBe` Right (Just FloatT)
  it "string literal gets StringT" $ do
    getLetType <$> typeCheckerHelper "let x = \"hello\";" `shouldBe` Right (Just StringT)
  it "true gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = true;" `shouldBe` Right (Just BoolT)
  it "false gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = false;" `shouldBe` Right (Just BoolT)
  it "null gets NullT" $ do
    getLetType <$> typeCheckerHelper "let x = null;" `shouldBe` Right (Just NullT)

testPrefixExpressions :: Spec
testPrefixExpressions = describe "Prefix expressions" $ do
  it "!true gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = !true;" `shouldBe` Right (Just BoolT)
  it "-5 gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = -5;" `shouldBe` Right (Just IntT)
  it "-3.14 gets FloatT" $ do
    getLetType <$> typeCheckerHelper "let x = -3.14;" `shouldBe` Right (Just FloatT)
  it "!5 is a type error" $ do
    case typeCheckerHelper "let x = !5;" of
      Left (TypeMismatch {}) -> return ()
      other -> expectationFailure $ "Expected TypeMismatch, got: " ++ show other

testInfixExpressions :: Spec
testInfixExpressions = describe "Infix expressions" $ do
  it "Int + Int gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = 1 + 2;" `shouldBe` Right (Just IntT)
  it "Float + Float gets FloatT" $ do
    getLetType <$> typeCheckerHelper "let x = 1.0 + 2.0;" `shouldBe` Right (Just FloatT)
  it "Int + Float gets FloatT" $ do
    getLetType <$> typeCheckerHelper "let x = 1 + 2.0;" `shouldBe` Right (Just FloatT)
  it "String + String gets StringT" $ do
    getLetType <$> typeCheckerHelper "let x = \"a\" + \"b\";" `shouldBe` Right (Just StringT)
  it "Int - Int gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = 5 - 3;" `shouldBe` Right (Just IntT)
  it "Int * Int gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = 4 * 2;" `shouldBe` Right (Just IntT)
  it "Int / Int gets IntT" $ do
    getLetType <$> typeCheckerHelper "let x = 10 / 2;" `shouldBe` Right (Just IntT)
  it "Int == Int gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = 1 == 1;" `shouldBe` Right (Just BoolT)
  it "Int != Int gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = 1 != 2;" `shouldBe` Right (Just BoolT)
  it "Int < Int gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = 1 < 2;" `shouldBe` Right (Just BoolT)
  it "Int > Int gets BoolT" $ do
    getLetType <$> typeCheckerHelper "let x = 2 > 1;" `shouldBe` Right (Just BoolT)
  it "String - String is a type error" $ do
    case typeCheckerHelper "let x = \"a\" - \"b\";" of
      Left (OperatorNotDefined {}) -> return ()
      other -> expectationFailure $ "Expected OperatorNotDefined, got: " ++ show other
  it "Int == String is a type error" $ do
    case typeCheckerHelper "let x = 1 == \"a\";" of
      Left (TypeMismatch {}) -> return ()
      other -> expectationFailure $ "Expected TypeMismatch, got: " ++ show other

testLetStatements :: Spec
testLetStatements = describe "Let statements" $ do
  it "annotation matches inferred type — passes" $ do
    getLetType <$> typeCheckerHelper "let x : Int = 5;" `shouldBe` Right (Just IntT)
  it "annotation mismatch — type error" $ do
    case typeCheckerHelper "let x : Int = \"hello\";" of
      Left (TypeMismatch {expected = IntT, got = StringT}) -> return ()
      other -> expectationFailure $ "Expected TypeMismatch Int/String, got: " ++ show other
  it "let binding is available in subsequent expression" $ do
    case typeCheckerHelper "let x = 5; let y = x;" of
      Right (TProgram stmts) -> case stmts of
        [_, TLetStatement _ _ _ ty] -> ty `shouldBe` IntT
        _ -> expectationFailure "Expected two let statements"
      Left err -> expectationFailure (show err)
