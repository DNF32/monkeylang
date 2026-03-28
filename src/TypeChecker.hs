{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Ast (Expression (..), FieldDecl (FieldDecl), Param (..), Statement (..), TExpression (..), TParam (..), TStatement (..), getType)
import Control.Monad.State (MonadState (get), StateT, gets, lift, modify, put)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Token (Position, Token (..), TokenType (..))
import Types

-- ============================================================
-- Typechecker - literals only for now
-- ============================================================
--
--
--
collectStructDecl :: Statement -> Check Statement
collectStructDecl (Program stms) = do
  let allStructDefs =
        Map.fromList
          [ (name, Map.fromList ([(fieldName, fieldType) | FieldDecl _ fieldName fieldType <- fields]))
            | StructDecl _ name fields <- stms
          ]
  let filteredStms = filter (not . isStructDecl) stms
  modify (\env -> env {typeDefs = allStructDefs})
  return (Program filteredStms)
  where
    isStructDecl (StructDecl {}) = True
    isStructDecl _ = False

type Check a = StateT TypecheckEnv (Either TypeError) a

-- typecheck :: TypecheckEnv -> Expression -> Either TypeError TExpression
-- typecheck env expr = case expr of
--  IntLit tok v -> Right $ TIntLit tok v IntT
--  FloatLit tok v -> Right $ TFloatLit tok v FloatT
--  StringLit tok v -> Right $ TStringLit tok v StringT
--  BooleanLit tok v -> Right $ TBoolLit tok v BoolT
--  NullLit tok -> Right $ TNullLit tok NullT
--  _ -> Left $ TypeMismatch AnyT AnyT -- todo

typeCheck :: Expression -> Check TExpression
typeCheck (IntLit tok v) = return (TIntLit tok v IntT)
typeCheck (FloatLit tok v) = return (TFloatLit tok v FloatT)
typeCheck (StringLit tok v) = return (TStringLit tok v StringT)
typeCheck (BooleanLit tok v) = return (TBoolLit tok v BoolT)
typeCheck (NullLit tok) = return (TNullLit tok NullT)
typeCheck (IdentifierLit tok name) = do
  maybeType <- gets (lookupVar name)
  case maybeType of
    Just ty -> return (TIdentifierLit tok name ty)
    Nothing -> lift $ Left $ InternalError ("Tried to get type of var:" ++ show name) Nothing
typeCheck (PrefixExpression tok@(Token _ pos) operator right) = do
  tRight <- typeCheck right
  let rightType = getType tRight
  resultType <- lift $ case (operator, rightType) of
    (Bang, BoolT) -> Right BoolT
    (Bang, AnyT) -> Right BoolT
    (Minus, IntT) -> Right IntT
    (Minus, FloatT) -> Right FloatT
    (Minus, AnyT) -> Right AnyT
    _ -> Left $ TypeMismatch {expected = AnyT, got = rightType, pos = Just pos}
  return $ TPrefixExpression tok operator tRight resultType
typeCheck (InfixExpression tok@(Token _ pos) left operator right) = do
  leftSide <- typeCheck left
  rightSide <- typeCheck right
  let leftType = getType leftSide
  let rightType = getType rightSide
  let opStr = show operator
  typeOfInfixNode <- lift $ case operator of
    Plus -> updatePos $ typeCheckSum leftType rightType
    Minus -> updatePos $ typeCheckArith opStr leftType rightType
    Asterisk -> updatePos $ typeCheckArith opStr leftType rightType
    Slash -> updatePos $ typeCheckArith opStr leftType rightType
    Equal -> updatePos $ typeCheckEquality leftType rightType
    NotEqual -> updatePos $ typeCheckEquality leftType rightType
    LessThan -> updatePos $ typeCheckComparison opStr leftType rightType
    GreaterThan -> updatePos $ typeCheckComparison opStr leftType rightType
    _ -> Right AnyT
  return $ TInfixExpression tok leftSide operator rightSide typeOfInfixNode
  where
    updatePos :: Either TypeError Type -> Either TypeError Type
    updatePos (Left err) = Left (err {pos = Just pos})
    updatePos (Right t) = Right t
typeCheck (IfExpression tok condition consequence alternative) = do
  tCondition <- typeCheck condition
  tConsequence <- mapM statementTypeChecker consequence
  tAlternative <- mapM (mapM statementTypeChecker) alternative
  return $ TIfExpression tok tCondition tConsequence tAlternative VoidT
typeCheck (FunctionLit tok params returnType body) = do
  oldEnv <- get
  modify (extendedTypeEnv params)
  typedBody <- mapM statementTypeChecker body
  retStack <- gets currentRetTy
  put oldEnv -- restore BEFORE returning, always
  inferredRetT <- simplifyRetTy <$> (mapM resolveType retStack)
  tParams <- mapM toTParam params
  let paramTypes = map (\(TParam _ _ ty) -> ty) tParams
  case returnType of
    Just annT
      | inferredRetT == annT ->
          return $ TFunctionLit tok tParams annT typedBody (FnT paramTypes annT)
      | otherwise ->
          lift $
            Left $
              TypeMismatch
                { expected = annT,
                  got = inferredRetT, -- more accurate than filterhead
                  pos = Just (_tokenPosition tok)
                }
    Nothing ->
      return $ TFunctionLit tok tParams inferredRetT typedBody (FnT paramTypes inferredRetT)
  where
    toTParam :: Param -> Check TParam
    toTParam (Param t n maybeT) = do
      resolvedT <- case maybeT of
        Just (UnresolvedT _) -> resolveType (fromJust maybeT)
        Just ty -> return ty
        Nothing -> return AnyT
      return (TParam t n resolvedT)
typeCheck _ = lift (Left $ InvalidParam Nothing)

extendedTypeEnv :: [Param] -> TypecheckEnv -> TypecheckEnv
extendedTypeEnv params env =
  foldl go env params
  where
    go :: TypecheckEnv -> Param -> TypecheckEnv
    go env (Param _ name maybeType) = insertVar name (fromMaybe AnyT maybeType) env

mkMismatch :: Type -> Type -> Maybe Position -> Either TypeError Type
mkMismatch l r p = Left $ TypeMismatch {expected = l, got = r, pos = p}

mkOpError :: String -> Type -> Type -> Either TypeError Type
mkOpError op l r = Left $ OperatorNotDefined op l r Nothing

-- We assume 'pos' is passed in from the caller (the InfixExpression token)
resolveType :: Type -> Check Type
resolveType (UnresolvedT name) = do
  structDefs <- gets typeDefs
  if Map.member name structDefs
    then
      return (StructT name)
    else
      lift $
        Left $
          UndefinedStruct
            { undefinedName = name,
              pos = Nothing
            }
resolveType ty = return ty

typeCheckSum :: Type -> Type -> Either TypeError Type
typeCheckSum IntT IntT = Right IntT
typeCheckSum FloatT FloatT = Right FloatT
typeCheckSum IntT FloatT = Right FloatT
typeCheckSum FloatT IntT = Right FloatT
typeCheckSum StringT StringT = Right StringT
typeCheckSum AnyT _ = Right AnyT
typeCheckSum _ AnyT = Right AnyT
typeCheckSum l r = mkMismatch l r Nothing

typeCheckArith :: String -> Type -> Type -> Either TypeError Type
typeCheckArith op IntT IntT = Right IntT
typeCheckArith op FloatT FloatT = Right FloatT
typeCheckArith op IntT FloatT = Right FloatT
typeCheckArith op FloatT IntT = Right FloatT
typeCheckArith op AnyT _ = Right AnyT
typeCheckArith op _ AnyT = Right AnyT
typeCheckArith op l r = mkOpError op l r

typeCheckEquality :: Type -> Type -> Either TypeError Type
typeCheckEquality AnyT _ = Right BoolT
typeCheckEquality _ AnyT = Right BoolT
-- Strict equality: types must match exactly (or be compatible via AnyT handled above)
typeCheckEquality t1 t2
  | t1 == t2 = Right BoolT
  | otherwise = mkMismatch t1 t2 Nothing

typeCheckComparison :: String -> Type -> Type -> Either TypeError Type
typeCheckComparison op IntT IntT = Right BoolT
typeCheckComparison op FloatT FloatT = Right BoolT
typeCheckComparison op IntT FloatT = Right BoolT
typeCheckComparison op FloatT IntT = Right BoolT
typeCheckComparison op AnyT _ = Right BoolT
typeCheckComparison op _ AnyT = Right BoolT
-- Explicit error for invalid comparison (e.g., comparing String < Int)
typeCheckComparison op l r = mkOpError op l r

statementTypeChecker :: Statement -> Check TStatement
statementTypeChecker (Program stmts) = do
  tStmts <- mapM statementTypeChecker stmts
  return (TProgram tStmts)
statementTypeChecker (LetStatement tok (IdentifierLit identtok@(Token tType pos) name) expr ann) = do
  typedExpr <- typeCheck expr
  let typeExpr = getType typedExpr

  case ann of
    Just annType
      | annType /= typeExpr -> lift (Left $ TypeMismatch {expected = annType, got = typeExpr, pos = Just pos})
      | otherwise -> do
          modify (insertVar name typeExpr)
          return (TLetStatement tok (TIdentifierLit identtok name typeExpr) typedExpr typeExpr)
    Nothing -> do
      modify (insertVar name typeExpr)
      return (TLetStatement tok (TIdentifierLit identtok name typeExpr) typedExpr typeExpr)
statementTypeChecker (ReturnStatement tok expr) = do
  typedExpr <- typeCheck expr
  modify (pushRetTy (getType typedExpr))
  return (TReturnStatement tok typedExpr)
statementTypeChecker (ExpressionStatement tok expr) = do
  typedExpr <- typeCheck expr
  return (TExpressionStatement tok typedExpr)
statementTypeChecker (BlockStatement stmts) = do
  tStmts <- mapM statementTypeChecker stmts
  return (TBlockStatement tStmts)
