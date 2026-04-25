{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Ast (Statement (..), TExpression (..), TStatement (..), prettyPrintStatement)
import Control.Monad.State.Lazy
import Data.Set qualified as Set
import Parser
import Test.Hspec
import Token (LexerState (..), Position (..), TokenType (Union))
import TypeChecker
import Types

main :: IO ()
main = hspec $ do
  testLiterals
  testPrefixExpressions
  testInfixExpressions
  testLetStatements
  testFunctionTypeChecking
  testIfBranchingTypeChecking
  testIfBranchingTypeNarrowing

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

typeCheckerHelper :: String -> Either TypeError (TStatement, TypecheckEnv)
typeCheckerHelper program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog@(Program _), _) ->
          runStateT (do newProg <- structResolutionPhase prog; statementTypeChecker newProg) emptyEnv
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
        Right (TProgram stmts, _) -> case head stmts of
          TLetStatement _ (TIdentifierLit _ _ ty) _ _ -> ty `shouldBe` (Binding Immutable (FnT [Binding Immutable IntT] StringT))
          _ -> expectationFailure "Expected TLetStatement"
        Left err -> expectationFailure (show err)
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
        Right (TProgram stmts, _) -> case head stmts of
          TLetStatement _ (TIdentifierLit _ name ty) _ _ -> ty `shouldBe` (Binding Immutable (FnT [Binding Immutable IntT] StringT))
          _ -> expectationFailure "Expected TLetStatement"
        Left _ -> expectationFailure "Expected typechecked  but TypeMismatch  successfully"
    it "Should infer UnionT when if branches return different types" $ do
      let program = "let x = fn() { if (true) { return 5; } return \"hello\"; }"
      case typeCheckerHelper program of
        Right (TProgram stmts, _) -> case head stmts of
          TLetStatement _ (TIdentifierLit _ name ty) _ _ -> ty `shouldBe` Binding Immutable (FnT [] (UnionT (Set.fromList [StringT, IntT])))
          _ -> expectationFailure "Expected TLetStatement"
        Left err -> expectationFailure (show err)
  describe "Resolve a struct type hint" $ do
    it "Resolved the return type to the Struct Foo" $ do
      let program = "Struct Foo { x: Int; y: Int; }; let x = fn(x: Int): Foo {return Foo {x: x; y: x;}}"
      case typeCheckerHelper program of
        Right (TProgram stmts, _) -> case head stmts of
          TLetStatement _ (TIdentifierLit _ name ty) _ _ -> ty `shouldBe` Binding Immutable (FnT [Binding Immutable IntT] (StructT "Foo"))
          _ -> expectationFailure "expected tletstatement"
        Left err -> expectationFailure (show err)
    it "Resolved the return type to the Struct Foo" $ do
      let program = "Struct Foo { x: Union[Foo,Null,Int]; y:Int; }; let x = fn(x: Int): Foo {return Foo {x: x; y: x;}}"
      case typeCheckerHelper program of
        Right (TProgram stmts, _) -> case head stmts of
          TLetStatement _ (TIdentifierLit _ name ty) _ _ -> ty `shouldBe` Binding Immutable (FnT [Binding Immutable IntT] (StructT "Foo"))
          _ -> expectationFailure "expected tletstatement"
        Left err -> expectationFailure (show err)

testIfBranchingTypeChecking :: Spec
testIfBranchingTypeChecking = do
  describe "Test if branching type checking" $ do
    it "Typechecks branch-local lets without leaking" $ do
      let program = "if (1){\n  let y = 10;\n}else{\n  let y= \"This is the end\"\n}"
      case typeCheckerHelper program of
        Right _ -> pure ()
        Left err -> expectationFailure (show err)

-- Additional type narrowing tests
testIfBranchingTypeNarrowing :: Spec
testIfBranchingTypeNarrowing = do
  describe "If branching type narrowing (advanced)" $ do
    it "Should narrow to falsy type in true branch of negation" $ do
      let program = "let x: Union[String, Int, Null] = Null; if (!x) { let y: Int = 10; } else { let y: String = \"hello\"; }"
      case typeCheckerHelper program of
        Right _ -> pure ()
        Left err -> expectationFailure (show err)
    it "Should narrow progressively through nested conditions" $ do
      let program = "let x: Union[String, Int, Null] = 5; let z = if (x) { if (x) { 10 } else { 10 } } else { 10 }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) -> do
          case lookupVar "z" env of
            Just ty -> ty `shouldBe` Binding Immutable IntT
            _ -> expectationFailure "Expected TLetStatement"
        Left err -> expectationFailure (show err)
    it "narrowing allows branch-typed lets for null check" $ do
      let program =
            "let x: Union[Int, Null] = Null; if (x==Null) { let y: Null = x; } else { let y: Int = x; }"
      case typeCheckerHelper program of
        Right _ -> pure ()
        Left err -> expectationFailure (show err)
    it "Should narrow null conditions" $ do
      let program = "let x: Union[Int, Null] = Null; let y = if (x!=Null) { x } else { 10 }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "y" env of
            Just ty -> ty `shouldBe` Binding Immutable IntT
            _ -> expectationFailure ("Expected y to have union type, got: " ++ show (lookupVar "y" env))
        Left err -> expectationFailure (show err)
    it "Should narrow for isInt" $ do
      let program = "let x: Union[Int, Null] = Null; let y = if (isInt(x)) { x } else { 10 }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "y" env of
            Just ty -> ty `shouldBe` Binding Immutable IntT
            _ -> expectationFailure ("Expected y to have union type, got: " ++ show (lookupVar "y" env))
        Left err -> expectationFailure (show err)
    it "Should narrow for isString" $ do
      let program = "let x: Union[String, Null] = Null; let y = if (isString(x)) { x } else { \"hello\" }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "y" env of
            Just ty -> ty `shouldBe` Binding Immutable StringT
            _ -> expectationFailure ("Expected y to have union type, got: " ++ show (lookupVar "y" env))
        Left err -> expectationFailure (show err)
    it "Impossible to narrow for isString and isInt" $ do
      let program = "let x: Union[String, Int,Null] = Null; let y = if (isString(x) && isInt(x)) { x } else { \"hello\" }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "y" env of
            Just ty -> ty `shouldBe` Binding Immutable StringT
            _ -> expectationFailure ("Expected y to have union type, got: " ++ show (lookupVar "y" env))
        Left err -> expectationFailure (show err)
    it "Should error if condition is always false" $ do
      let program = "if (null) { let y = 10; }"
      case typeCheckerHelper program of
        Left _ -> return () -- Expected to fail
        Right _ -> expectationFailure "Should not typecheck (always false condition)"
    it "Typechecks if-expression used in let binding" $ do
      let program = "let x = if (true) { 1 } else { 2 };"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "x" env of
            Just ty -> ty `shouldBe` Binding Immutable IntT
            _ -> expectationFailure "Expected let-bound x"
        Left err -> expectationFailure (show err)
    it "Should narrow null conditions" $ do
      let program = "let x: Union[Int, Null] = Null; let y = if (x!=Null && isString(x)) { x } else { 10 }"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "y" env of
            Just ty -> ty `shouldBe` Binding Immutable IntT
            _ -> expectationFailure ("Expected y to have union type, got: " ++ show (lookupVar "y" env))
        Left err -> expectationFailure (show err)

testCallExprTyping :: Spec
testCallExprTyping = do
  describe "Type check call expression" $ do
    it "Should narrow to falsy type in true branch of negation" $ do
      let program =
            "let x: Union[String, Int, Null] = Null; let foo = fn (x:Int, y: Union[String, Int]){return y;} foo(1,\"That\");"
      case typeCheckerHelper program of
        Right (TProgram _stmts, env) ->
          case lookupVar "foo" env of
            Just ty ->
              ty
                `shouldBe` Binding
                  Immutable
                  ( FnT
                      [ Binding Immutable IntT,
                        Binding Immutable (UnionT (Set.fromList [IntT, StringT]))
                      ]
                      (UnionT (Set.fromList [IntT, StringT]))
                  )
            _ -> expectationFailure "Expected TLetStatement"
        Left err -> expectationFailure (show err)
    it "rejects passing Union to Int param" $ do
      let program =
            "let x: Union[String, Int, Null] = Null; let foo = fn (x:Int, y: Union[String, Int]){return y;} foo(x,\"That\");"
      case typeCheckerHelper program of
        Left (TypeMismatch {expected = IntT, got = UnionT set}) -> do
          set `shouldBe` Set.fromList [IntT, StringT, NullT]
        other -> expectationFailure ("Expected TypeMismatch IntT, got: " ++ show other)
    it "Channing of function calls with type mismatch" $ do
      let program =
            "let x: Union[String, Int, Null] = Null; let bar = fn(x:String){return x;}; let foo = fn (x:Int){return x;} foo(bar(x));"
      case typeCheckerHelper program of
        Left (TypeMismatch {expected = StringT, got = UnionT set}) -> do
          set `shouldBe` Set.fromList [IntT, StringT, NullT]
        other -> expectationFailure ("Expected TypeMismatch StringT, got: " ++ show other)
    it "Channing of function calls without type mismatch" $ do
      let program =
            "let x = 5; let bar = fn(x:Int){return x;}; let foo = fn (x:Int){return x;} foo(bar(x));"
      case typeCheckerHelper program of
        Right _ -> pure ()
        other -> expectationFailure ("Expected program to typecheck")

-- helpers

getLetType :: TStatement -> Maybe Type
getLetType (TProgram (TLetStatement _ _ _ ty : _)) = Just $ getType ty
getLetType _ = Nothing

testLiterals :: Spec
testLiterals = describe "Literals" $ do
  it "integer literal gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 5;" `shouldBe` Right (Just IntT)
  it "float literal gets FloatT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 3.14;" `shouldBe` Right (Just FloatT)
  it "string literal gets StringT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = \"hello\";" `shouldBe` Right (Just StringT)
  it "true gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = true;" `shouldBe` Right (Just BoolT)
  it "false gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = false;" `shouldBe` Right (Just BoolT)
  it "null gets NullT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = Null;" `shouldBe` Right (Just NullT)

testPrefixExpressions :: Spec
testPrefixExpressions = describe "Prefix expressions" $ do
  it "!true gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = !true;" `shouldBe` Right (Just BoolT)
  it "-5 gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = -5;" `shouldBe` Right (Just IntT)
  it "-3.14 gets FloatT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = -3.14;" `shouldBe` Right (Just FloatT)
  it "!5 gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = !5;" `shouldBe` Right (Just BoolT)

testInfixExpressions :: Spec
testInfixExpressions = describe "Infix expressions" $ do
  it "Int + Int gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1 + 2;" `shouldBe` Right (Just IntT)
  it "Float + Float gets FloatT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1.0 + 2.0;" `shouldBe` Right (Just FloatT)
  it "Int + Float gets FloatT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1 + 2.0;" `shouldBe` Right (Just FloatT)
  it "String + String gets StringT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = \"a\" + \"b\";" `shouldBe` Right (Just StringT)
  it "Int - Int gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 5 - 3;" `shouldBe` Right (Just IntT)
  it "Int * Int gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 4 * 2;" `shouldBe` Right (Just IntT)
  it "Int / Int gets IntT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 10 / 2;" `shouldBe` Right (Just IntT)
  it "Int == Int gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1 == 1;" `shouldBe` Right (Just BoolT)
  it "Int != Int gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1 != 2;" `shouldBe` Right (Just BoolT)
  it "Int < Int gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 1 < 2;" `shouldBe` Right (Just BoolT)
  it "Int > Int gets BoolT" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x = 2 > 1;" `shouldBe` Right (Just BoolT)
  it "String - String is a type error" $ do
    case typeCheckerHelper "let x = \"a\" - \"b\";" of
      Left (OperatorNotDefined {}) -> return ()
      other -> expectationFailure $ "Expected OperatorNotDefined"
  it "Int == String is a type error" $ do
    case typeCheckerHelper "let x = 1 == \"a\";" of
      Left (TypeMismatch {}) -> return ()
      other -> expectationFailure $ "Expected TypeMismatch"

testLetStatements :: Spec
testLetStatements = describe "Let statements" $ do
  it "annotation matches inferred type — passes" $ do
    getLetType <$> fst <$> typeCheckerHelper "let x : Int = 5;" `shouldBe` Right (Just IntT)
  it "annotation mismatch — type error" $ do
    case typeCheckerHelper "let x : Int = \"hello\";" of
      Left (TypeMismatch {expected = IntT, got = StringT}) -> return ()
      other -> expectationFailure $ "Expected TypeMismatch Int/String, got: " ++ show other
  it "let binding is available in subsequent expression" $ do
    case typeCheckerHelper "let x = 5; let y = x;" of
      Right (TProgram stmts, _) -> case stmts of
        [_, TLetStatement _ _ _ ty] -> ty `shouldBe` (Binding Immutable IntT)
        _ -> expectationFailure "Expected two let statements"
      Left err -> expectationFailure (show err)
