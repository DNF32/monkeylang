{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Ast (Expression (..), FieldDecl (FieldDecl, fieldName), FieldInitialization (FieldInit), Param (..), Statement (..), TExpression (..), TFieldInitialization (TFieldInit), TParam (..), TStatement (..), getType)
import Control.Monad.State (MonadState (get), StateT, gets, lift, modify, put)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as Set
import Token (Position, Token (..), TokenType (..), getPos)
import Types

-- ============================================================
-- Typechecker - literals only for now
-- ============================================================
--
--
--
structResolutionPhase :: Statement -> Check Statement
structResolutionPhase prog@(Program stms) = do
  prog' <- collectStructDecl prog
  resolveAllStructs
  checkCycles
  return prog'

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
  -- Resolve all UnresolvedT references in struct definitions
  resolveAllStructs
  return (Program filteredStms)
  where
    isStructDecl (StructDecl {}) = True
    isStructDecl _ = False

resolveAllStructs :: Check ()
resolveAllStructs = do
  env <- get
  let typeDefs' = typeDefs env
  resolvedDefs <- traverse resolveStructDef typeDefs'
  modify (\e -> e {typeDefs = resolvedDefs})

checkCycles :: Check ()
checkCycles = do
  env <- get
  _ <- Map.traverseWithKey (\k v -> checkNoProblematicCycles k v) (typeDefs env)
  return ()

resolveStructDef :: StructDef -> Check StructDef
resolveStructDef fieldMap = traverse resolveType fieldMap

checkNoProblematicCycles :: StructName -> StructDef -> Check ()
checkNoProblematicCycles structName structDef = do
  if any (fieldAsCycleType structName) (Map.elems structDef)
    then lift $ Left $ InternalError {message = "Non-optional recursive reference in struct " ++ structName, pos = Nothing}
    else return ()
  where
    fieldAsCycleType :: String -> Type -> Bool
    fieldAsCycleType sName (UnresolvedT name) = sName == name
    fieldAsCycleType sName (StructT name) = sName == name
    fieldAsCycleType sName unionTy@(UnionT _) = (unionHas (UnresolvedT sName) unionTy || unionHas (StructT sName) unionTy) && not (isOptional unionTy)
    fieldAsCycleType _ _ = False

-- TODO: We need to check to check if we have or not an optional in there If we do there isn't a cycle

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
typeCheck (FieldAccess tok@(Token _ pos) object fieldName) = do
  tObject <- typeCheck object
  let objType = getType tObject
  case objType of
    (StructT structName) -> do
      fieldTy <- gets (lookupFieldType structName fieldName)
      case fieldTy of
        Just ty -> return $ TFieldAccess tok tObject fieldName ty
        Nothing ->
          lift $
            Left $
              UndefinedField
                { structName = structName,
                  undefinedFieldName = fieldName,
                  pos = Just pos
                }
    _ ->
      lift $
        Left $
          TypeMismatch
            { expected = AnyT,
              got = objType,
              pos = Just pos
            }
typeCheck (StructInitialization tok sName fieldInits) = do
  tFieldInits <- mapM (lookupField sName) fieldInits
  return $ TStructInitialization tok sName tFieldInits (StructT sName)
  where
    lookupField :: String -> FieldInitialization -> Check TFieldInitialization
    lookupField sName (FieldInit tok fname initValue) = do
      tExpr <- typeCheck initValue
      let foundTy = getType tExpr
      fieldTy <- gets (lookupFieldType sName fname)
      case fieldTy of
        Just ty
          | isCompatible ty foundTy -> return (TFieldInit tok fname tExpr ty)
          | otherwise ->
              lift $
                Left $
                  TypeMismatch
                    { expected = ty,
                      got = foundTy,
                      pos = Just (_tokenPosition tok)
                    }
        Nothing ->
          lift $
            Left $
              UndefinedField
                { structName = sName,
                  undefinedFieldName = fname,
                  pos = Just (getPos tok)
                }
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
    Just annT -> do
      resolvedAnnT <- resolveType annT
      if isCompatible inferredRetT resolvedAnnT
        then return $ TFunctionLit tok tParams resolvedAnnT typedBody (FnT paramTypes inferredRetT)
        else
          lift $
            Left $
              TypeMismatch
                { expected = resolvedAnnT,
                  got = inferredRetT,
                  pos = Just (getPos tok)
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
resolveType (UnionT s) = do
  resolvedTypes <- traverse resolveType (Set.toList s)
  return (UnionT (Set.fromList resolvedTypes))
resolveType (ArrayT elemType) = do
  resolvedElemType <- resolveType elemType
  return (ArrayT resolvedElemType)
resolveType (FnT paramTypes retType) = do
  resolvedParamTypes <- traverse resolveType paramTypes
  resolvedRetType <- resolveType retType
  return (FnT resolvedParamTypes resolvedRetType)
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

branchEnv :: TypecheckEnv -> TExpression -> (TypecheckEnv, TypecheckEnv)
branchEnv = undefined

-- branchEnv initialEnv (TInfixExpression tok left NotEqual right ty)  = do
--  let lTy =getType left
--  let rTy = getType right
--  if isCompatible lTy rTy || isCompatible rTy lTy
--
-- branchEnv initialEnv (TIdentifierLit tok name ty) = do
--
--    if canBeTruthy ty
--      then

truthyType :: Type -> Maybe Type
truthyType (UnionT set) = Just (UnionT (Set.filter canBeTruthy set))
truthyType VoidT = Nothing
truthyType NullT = Nothing
truthyType ty = Just ty

falsyType :: Type -> Maybe Type
falsyType (UnionT set) = Just (UnionT (Set.filter canBeFalsy set))
falsyType ty =
  if canBeFalsy ty
    then Just ty
    else Nothing

canBeTruthy :: Type -> Bool
canBeTruthy NullT = False -- Null is always falsy
canBeTruthy BoolT = True -- Bool can be true
canBeTruthy IntT = True -- Int can be non-zero (truthy)
canBeTruthy StringT = True -- String can be non-empty (truthy)
canBeTruthy (ArrayT _) = True -- Array can be non-empty (truthy)
canBeTruthy VoidT = False -- Void is always falsy
canBeTruthy _ = True -- Others: assume can be truthy

canBeFalsy :: Type -> Bool
canBeFalsy NullT = True -- Null is falsy
canBeFalsy BoolT = True -- Bool can be false
canBeFalsy IntT = True -- Int can be zero (falsy)
canBeFalsy StringT = True -- String can be empty (falsy)
canBeFalsy (ArrayT _) = True -- Array can be empty (falsy)
canBeFalsy VoidT = True -- Void is falsy
canBeFalsy _ = True -- Others: assume can be falsy
