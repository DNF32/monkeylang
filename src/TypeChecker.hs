{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Ast (Expression (..), FieldDecl (..), FieldInitialization (FieldInit), Param (..), Statement (..), TExpression (..), TFieldInitialization (TFieldInit), TParam (..), TStatement (..))
import Control.Monad.State (MonadState (get), StateT, gets, lift, modify, put)
import Control.Monad.State.Lazy
import Data.Bifunctor (first)
import Data.IntMap.Merge.Lazy (zipWithMatched)
import Data.Map qualified as Map
import Data.Map.Merge.Strict qualified as Merge
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.ExecutionStack (Location (objectName))
import Parser
import Token (HasPos (getMaybePos), Position (..), Token (..), TokenType (..), getPos, getToken, initialState)
import Types

-- ============================================================
-- Typechecker - literals only for now
-- ============================================================
--
--
typeChecker :: String -> Either TypeError (TStatement, TypecheckEnv)
typeChecker program =
  let state = initialState program
   in case runAstParser parseProgram state of
        Right (prog@(Program _), _) ->
          runStateT (do newProg <- structResolutionPhase prog; statementTypeChecker newProg) emptyEnv
        Left parseErr ->
          Left $ InternalError {message = show parseErr, pos = Nothing}

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
  let numericType = UnionT (Set.fromList [IntT, FloatT])
  resultType <- lift $ case (operator, rightType) of
    (Bang, _) -> Right BoolT -- Bang works on any type, returns Bool
    (Minus, IntT) -> Right IntT
    (Minus, FloatT) -> Right FloatT
    _ -> Left $ TypeMismatch {expected = numericType, got = rightType, pos = Just pos}
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
    And -> updatePos $ typeCheckLogical leftType rightType
    Or -> updatePos $ typeCheckLogical leftType rightType
    _ -> Left $ InternalError {message = "Unhandled infix operator: " ++ show operator, pos = Just pos}
  return $ TInfixExpression tok leftSide operator rightSide typeOfInfixNode
  where
    updatePos :: Either TypeError Type -> Either TypeError Type
    updatePos (Left err) = Left (err {pos = Just pos})
    updatePos (Right t) = Right t
typeCheck (IfExpression tok condition consequence alternative) = do
  tCondition <- typeCheck condition
  baseEnv <- get
  let (truthyEnv, falsyEnv) = branchEnv baseEnv tCondition
  truthNode <- nodeFromData truthyEnv consequence
  put baseEnv
  falsyNode <- case alternative of
    Nothing -> pure Unreachable
    Just stmts -> nodeFromData falsyEnv stmts
  case (truthNode, falsyNode) of
    (Unreachable, Unreachable) ->
      lift $ Left $ InternalError {message = "If condition has no reachable branches", pos = Just (getPos tok)}
    (BranchNode tStmt truthEnv nodeType, Unreachable) -> do
      let ifExprTy = simplifyRetTy $ (: []) $ case nodeType of
            Expr ty -> ty
            Ret _ -> NeverT
            Void -> VoidT
      put truthEnv
      return $ TIfExpression tok tCondition (Just tStmt) Nothing ifExprTy
    (Unreachable, BranchNode tStmt falseEnv nodeType) -> do
      let ifExprTy = simplifyRetTy $ (: []) $ case nodeType of
            Expr ty -> ty
            Ret _ -> NeverT
            Void -> VoidT
      put falseEnv
      return $ TIfExpression tok tCondition Nothing (Just tStmt) ifExprTy
    (BranchNode tConsequence truthEnv truthNodeType, BranchNode tAlternative falseEnv falseNodeType) -> do
      (ifExprTy, newEnv) <-
        first (simplifyRetTy . (: []))
          <$> ( case (truthNodeType, falseNodeType) of
                  -- both branches return: if-expression never produces a value
                  (Ret _, Ret _) ->
                    return (NeverT, baseEnv) -- we dont even need an even since there cant be any exec after
                    -- one branch returns, the other yields a value
                  (Expr ty, Ret _) ->
                    return (ty, truthEnv)
                  (Ret _, Expr ty) ->
                    return (ty, falseEnv)
                  -- one branch returns, the other yields unit
                  (Void, Ret _) ->
                    return (VoidT, truthEnv)
                  (Ret _, Void) ->
                    return (VoidT, falseEnv)
                  -- both branches yield values: must match
                  (Expr ty1, Expr ty2)
                    | ty1 == ty2 ->
                        return (ty1, mergeEnvs baseEnv truthEnv falseEnv)
                    | otherwise ->
                        lift $ Left $ TypeMismatch {expected = ty1, got = ty2, pos = Just (getPos tok)}
                  -- both branches yield unit
                  (Void, Void) ->
                    return (VoidT, mergeEnvs baseEnv truthEnv falseEnv)
                  -- mixed value/unit is a type error (Rust-like)
                  (Expr ty, Void) ->
                    lift $ Left $ TypeMismatch {expected = ty, got = VoidT, pos = Just (getPos tok)}
                  (Void, Expr ty) ->
                    lift $ Left $ TypeMismatch {expected = VoidT, got = ty, pos = Just (getPos tok)}
              )
      put newEnv
      return $ TIfExpression tok tCondition (Just tConsequence) (Just tAlternative) ifExprTy
  where
    mergeEnvs :: TypecheckEnv -> TypecheckEnv -> TypecheckEnv -> TypecheckEnv
    mergeEnvs baseEnv (TypecheckEnv tyEnv _ _ _ fieldEnv) (TypecheckEnv ty'Env _ _ _ field'Env) = baseEnv {typeEnv = mergeScopes tyEnv ty'Env, fieldEnv = mergeField fieldEnv field'Env}

    mergeScopes :: [TypeEnv] -> [TypeEnv] -> [TypeEnv]
    mergeScopes = zipWith mergeScope

    mergeScope :: TypeEnv -> TypeEnv -> TypeEnv
    mergeScope = Merge.merge Merge.preserveMissing Merge.preserveMissing (Merge.zipWithMatched matchBinding)

    mergeField :: Map.Map (String, String) Type -> Map.Map (String, String) Type -> Map.Map (String, String) Type
    mergeField = Merge.merge Merge.preserveMissing Merge.preserveMissing (Merge.zipWithMatched matchField)

    matchField :: (String, String) -> Type -> Type -> Type
    matchField _ ty ty' = simplifyRetTy $ [ty, ty']

    matchBinding :: String -> Binding -> Binding -> Binding
    matchBinding _ (Binding ty mut) (Binding tyy mutt) = Binding (simplifyRetTy [ty, tyy]) mut
--  outputEnv <- lift $ case (tConsequence ,tAlternative) of
--    (Nothing, Nothing)->
--    (tConsequence , Nothing) -> if truthHasReturn
--    Nothing -> Right outFalsyEnv
--    Just _ ->
--      case (truthyEnv, falsyEnv) of
--        (Nothing, _) -> Right outFalsyEnv -- truthy unreachable, use falsy
--        (_, Nothing) -> Right outTruthEnv -- falsy unreachable, use truthy
--        _ -> mergeEnvs outTruthEnv outFalsyEnv branchLessEnv
--  put (outputEnv {currentRetTy = truthyNewRetTy ++ falsyNewRetTy ++ baseRetTy})
--  return $ TIfExpression tok tCondition tConsequence tAlternative exprTy
--  where
--    controlFlow :: Maybe TStatement  -> Maybe TStatement -> Maybe
--    node
--    typeCheckBody :: Maybe TypecheckEnv -> [Statement] -> Check (Maybe TStatement, [Type])
--    typeCheckBody (Just env) stmts = do
--      put env
--      (typedStmt, ty) <- inferBlockReturnType stmts
--      return (typedStmt, ty)
--    typeCheckBody Nothing _ = return (Nothing, [])
--
--    typeCheckAlt :: Maybe TypecheckEnv -> Maybe [Statement] -> Check (Maybe TStatement, [Type])
--    typeCheckAlt _ Nothing = return (Nothing, [])
--    typeCheckAlt env (Just stmts) = typeCheckBody env stmts
--
--    takeNewReturns :: [Type] -> [Type] -> [Type]
--    takeNewReturns base current = take (length current - length base) current
--
--    mergeEnvs :: TypecheckEnv -> TypecheckEnv -> TypecheckEnv -> Either TypeError TypecheckEnv
--    mergeEnvs truthEnv falsyEnv base = do
--      let baseScope = case typeEnv base of
--            (scope : _) -> scope
--            [] -> Map.empty
--          truthScope = case typeEnv truthEnv of
--            (scope : _) -> scope
--            [] -> Map.empty
--          falsyScope = case typeEnv falsyEnv of
--            (scope : _) -> scope
--            [] -> Map.empty
--          baseKeys = Map.keysSet baseScope
--          truthExisting = Map.restrictKeys truthScope baseKeys
--          falsyExisting = Map.restrictKeys falsyScope baseKeys
--          mergedExisting = Map.intersectionWith mergeTy truthExisting falsyExisting
--          updatedBase = Map.union baseScope mergedExisting
--      Right $ base {typeEnv = updatedBase : drop 1 (typeEnv base)}
--      where
--        mergeTy t1 t2 =
--          let s1 = typeToSet t1
--              s2 = typeToSet t2
--              merged = Set.union s1 s2
--           in if Set.size merged == 1
--                then Set.findMin merged
--                else UnionT merged
--        typeToSet (UnionT s) = s
--        typeToSet t = Set.singleton t
typeCheck (FieldAccess tok@(Token _ pos) object fieldName) = do
  tObject <- typeCheck object
  let objType = getType tObject
  case objType of
    (StructT structName) -> do
      fieldTy <- gets (lookupFieldType structName fieldName)
      case fieldTy of
        Just ty -> do
          let refinedTy = case object of
                IdentifierLit _ varName ->
                  gets (lookupFieldRefinement varName fieldName)
                _ ->
                  pure Nothing
          finalTy <- refinedTy
          return $ TFieldAccess tok tObject fieldName (fromMaybe ty finalTy)
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
            { expected = StructT "<struct>",
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
          | isCompatible foundTy ty -> return (TFieldInit tok fname tExpr ty)
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
  tParams <- mapM toTParam params
  let paramBindings = [(name, binding) | (Param _ name _ _, binding) <- zip params (map paramToBinding tParams)]
  modify (extendTypeEnv paramBindings)
  typedBody <- mapM statementTypeChecker body
  retStack <- gets currentRetTy
  put oldEnv -- restore BEFORE returning, always
  inferredRetT <- simplifyRetTy <$> (mapM resolveType retStack)
  let bindings = map snd paramBindings
  case returnType of
    Just annT -> do
      resolvedAnnT <- resolveType annT
      if isCompatible inferredRetT resolvedAnnT
        then return $ TFunctionLit tok tParams resolvedAnnT typedBody (FnT bindings inferredRetT)
        else
          lift $
            Left $
              TypeMismatch
                { expected = resolvedAnnT,
                  got = inferredRetT,
                  pos = Just (getPos tok)
                }
    Nothing ->
      return $ TFunctionLit tok tParams inferredRetT typedBody (FnT bindings inferredRetT)
  where
    toTParam :: Param -> Check TParam
    toTParam (Param t n ty mut) = do
      finalTy <- resolveType ty
      return (TParam t n finalTy mut)
    paramToBinding :: TParam -> Binding
    paramToBinding (TParam t n ty mut) = Binding ty mut
typeCheck (CallExpression tok func args) = do
  tFunc <- typeCheck func
  tArgs <- mapM typeCheck args
  case getType tFunc of
    FnT paramTypes retType
      | length paramTypes /= length tArgs ->
          lift $ Left $ InvalidParam {pos = Just (getPos tok)}
      | otherwise ->
          let results = [(match param gotExpr, gotExpr) | (param, gotExpr) <- zip paramTypes tArgs]
              mismatched = filter (\(x, _) -> if x /= MatchSuccess then True else False) $ results
              errorData = map (\(x, y) -> BindingError x (getToken y)) mismatched
           in case errorData of
                (x : _) -> lift $ Left $ MissMatchOnCallable errorData
                [] ->
                  return (TCallExpression tok tFunc tArgs retType)
    AnyT -> return (TCallExpression tok tFunc tArgs AnyT)
    _ -> lift $ Left $ NotCallable {gotType = getType tFunc, pos = Just (getPos tok)}
  where
    match :: Binding -> TExpression -> MatchResult
    match (Binding expTy expMut) (TIdentifierLit _ _ (Binding gotTy gotMut)) =
      let tyRes = isCompatible expTy gotTy
          mutRes = expMut <= gotMut
       in case (tyRes, mutRes) of
            (True, True) -> MatchSuccess
            (True, False) -> MutabilityIncompatible
            (False, True) -> TypeIncompatible
            (False, False) -> IncompatibleBoth
    match (Binding expTy _) expr = if isCompatible expTy $ getType expr then MatchSuccess else TypeIncompatible
typeCheck _ = lift (Left $ InvalidParam Nothing)

extendTypeEnv :: [(String, Binding)] -> TypecheckEnv -> TypecheckEnv
extendTypeEnv bindings env =
  foldl (\acc (name, ty) -> defineBinding name ty acc) env bindings

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
  resolvedParamTypes <-
    traverse
      ( \(Binding t mut) ->
          do
            resTy <- resolveType t
            return (Binding resTy mut)
      )
      paramTypes
  resolvedRetType <- resolveType retType
  return (FnT resolvedParamTypes resolvedRetType)
resolveType ty = return ty

typeCheckSum :: Type -> Type -> Either TypeError Type
typeCheckSum IntT IntT = Right IntT
typeCheckSum FloatT FloatT = Right FloatT
typeCheckSum IntT FloatT = Right FloatT
typeCheckSum FloatT IntT = Right FloatT
typeCheckSum StringT StringT = Right StringT
typeCheckSum l r = mkMismatch l r Nothing

typeCheckArith :: String -> Type -> Type -> Either TypeError Type
typeCheckArith op IntT IntT = Right IntT
typeCheckArith op FloatT FloatT = Right FloatT
typeCheckArith op IntT FloatT = Right FloatT
typeCheckArith op FloatT IntT = Right FloatT
typeCheckArith op l r = mkOpError op l r

typeCheckEquality :: Type -> Type -> Either TypeError Type
-- Equality is allowed when types overlap
typeCheckEquality t1 t2 =
  case intersectType t1 t2 of
    Just _ -> Right BoolT
    Nothing -> mkMismatch t1 t2 Nothing

typeCheckComparison :: String -> Type -> Type -> Either TypeError Type
typeCheckComparison op IntT IntT = Right BoolT
typeCheckComparison op FloatT FloatT = Right BoolT
typeCheckComparison op IntT FloatT = Right BoolT
typeCheckComparison op FloatT IntT = Right BoolT
-- Explicit error for invalid comparison (e.g., comparing String < Int)
typeCheckComparison op l r = mkOpError op l r

typeCheckLogical :: Type -> Type -> Either TypeError Type
typeCheckLogical BoolT BoolT = Right BoolT
typeCheckLogical l r = mkMismatch l r Nothing

statementTypeChecker :: Statement -> Check TStatement
statementTypeChecker (Program stmts) = do
  tStmts <- mapM statementTypeChecker stmts
  return (TProgram tStmts)
statementTypeChecker (LetStatement tok (IdentifierLit identtok@(Token tType pos) name) expr ann mutPer) = do
  typedExpr <- typeCheck expr
  let typeExpr = getType typedExpr

  case ann of
    Just annType
      | not (isCompatible typeExpr annType) -> lift (Left $ TypeMismatch {expected = annType, got = typeExpr, pos = Just pos})
      | otherwise -> do
          modify (defineBinding name (Binding annType mutPer))
          return (TLetStatement tok (TIdentifierLit identtok name (Binding annType mutPer)) typedExpr (Binding annType mutPer))
    Nothing -> do
      modify (defineBinding name (Binding typeExpr mutPer))
      return (TLetStatement tok (TIdentifierLit identtok name (Binding typeExpr mutPer)) typedExpr (Binding typeExpr mutPer))
statementTypeChecker (ReturnStatement tok expr) = do
  typedExpr <- typeCheck expr
  modify (pushRetTy (getType typedExpr))
  return (TReturnStatement tok typedExpr)
statementTypeChecker (AssignmentStatement tok (IdentifierLit tok' name) expr) = do
  maybeBin <- lookupVar name
  case maybeBin  of
    Just (Binding ty mut) -> case mut of 
                              Immutable -> -- this needs to error
                              Mutable -> if isCompatible ty $ getType expr  then (interction of types) else error
    Nothing -> -- this needs to error
statementTypeChecker (ExpressionStatement tok expr) = do
  typedExpr <- typeCheck expr
  return (TExpressionStatement tok typedExpr)
statementTypeChecker (BlockStatement stmts) = do
  modify pushScope
  tStmts <- go stmts
  modify popScope
  return (TBlockStatement tStmts)
  where
    go [] = pure []
    go (s : ss) = do
      t <- statementTypeChecker s
      case t of
        TReturnStatement {} -> pure [t]
        _ -> (t :) <$> go ss

-- branchEnv = undefined

-- branchEnv initialEnv (TInfixExpression tok left NotEqual right ty)  = do
--  let lTy =getType left
--  let rTy = getType right
--  if isCompatible lTy rTy || isCompatible rTy lTy
--
--
branchEnv :: TypecheckEnv -> TExpression -> (Maybe TypecheckEnv, Maybe TypecheckEnv)
branchEnv initialEnv (TIdentifierLit tok name (Binding ty _)) = do
  let truthTy = truthyType ty
      falseTy = falsyType ty

      truthEnv =
        case truthTy of
          Just t -> Just (updateVar name t initialEnv)
          Nothing -> Nothing -- or unreachable branch
      falseEnv =
        case falseTy of
          Just t -> Just (updateVar name t initialEnv)
          Nothing -> Nothing -- or unreachable branch
  (truthEnv, falseEnv)
branchEnv initialEnv (TIntLit _ value _) = do
  if value /= 0
    then (Just initialEnv, Nothing)
    else (Nothing, Just initialEnv)
branchEnv initialEnv (TStringLit _ value _) = do
  if not (null value)
    then (Just initialEnv, Nothing)
    else (Nothing, Just initialEnv)
branchEnv initialEnv (TBoolLit _ value _) = do
  if value
    then (Just initialEnv, Nothing)
    else (Nothing, Just initialEnv)
branchEnv initialEnv (TNullLit _ _) = (Nothing, Just initialEnv)
branchEnv initialEnv (TFloatLit {}) = (Just initialEnv, Nothing)
branchEnv initialEnv (TPrefixExpression _ Bang right _) = do
  let (truthyEnv, falsyEnv) = branchEnv initialEnv right
  (falsyEnv, truthyEnv)
branchEnv initialEnv (TInfixExpression _ left And right _) =
  case branchEnv initialEnv left of
    (Nothing, falsyEnv) -> (Nothing, falsyEnv)
    (Just truthyEnv, falsyEnvLeft) ->
      let (truthyEnvRight, falsyEnvRight) = branchEnv truthyEnv right
          falsyEnv = mergeBranchEnvs initialEnv falsyEnvLeft falsyEnvRight
       in (truthyEnvRight, falsyEnv)
branchEnv initialEnv (TInfixExpression _ left Or right _) =
  case branchEnv initialEnv left of
    (truthyEnvLeft, Nothing) -> (truthyEnvLeft, Nothing)
    (truthyEnvLeft, Just falsyEnvLeft) ->
      let (truthyEnvRight, falsyEnvRight) = branchEnv falsyEnvLeft right
          truthyEnv = mergeBranchEnvs initialEnv truthyEnvLeft truthyEnvRight
       in (truthyEnv, falsyEnvRight)
branchEnv initialEnv (TInfixExpression _ left Equal right _) =
  case (left, right) of
    (TIdentifierLit _ _ lTy, TNullLit _ _) ->
      let truthEnv = Just (narrowEnv initialEnv left NullT)
          falsyEnv = case bindingNullType lTy of
            Just (Binding narrowed _) -> Just (narrowEnv initialEnv left narrowed)
            Nothing -> Nothing
       in (truthEnv, falsyEnv)
    (TNullLit _ _, TIdentifierLit _ _ rTy) ->
      let truthEnv = Just (narrowEnv initialEnv right NullT)
          falsyEnv = case bindingNullType rTy of
            Just (Binding narrowed _) -> Just (narrowEnv initialEnv right narrowed)
            Nothing -> Nothing
       in (truthEnv, falsyEnv)
    _ ->
      case intersectType (getType left) (getType right) of
        Nothing ->
          (Nothing, Just initialEnv) -- true branch unreachable
        Just narrowTy ->
          let truthEnv = narrowEnv (narrowEnv initialEnv left narrowTy) right narrowTy
           in (Just truthEnv, Just initialEnv)
branchEnv env (TInfixExpression _ left NotEqual right _) =
  case (left, right) of
    (TIdentifierLit _ _ lTy, TNullLit _ _) ->
      let truthEnv = case bindingNullType lTy of
            Just (Binding narrowed _) -> Just (narrowEnv env left narrowed)
            Nothing -> Nothing
          falsyEnv = Just (narrowEnv env left NullT)
       in (truthEnv, falsyEnv)
    (TNullLit _ _, TIdentifierLit _ _ rTy) ->
      let truthEnv = case bindingNullType rTy of
            Just (Binding narrowed _) -> Just (narrowEnv env right narrowed)
            Nothing -> Nothing
          falsyEnv = Just (narrowEnv env right NullT)
       in (truthEnv, falsyEnv)
    _ ->
      case intersectType (getType left) (getType right) of
        Nothing -> (Just env, Nothing) -- always true
        Just _ -> (Just env, Just env) -- no narrowing
branchEnv env (TCallExpression _ (TIdentifierLit _ "isStruct" _) [TIdentifierLit _ structName _, var@(TIdentifierLit _ varName varTy)] _) =
  let structTy = StructT structName
   in case lookupVar varName env of
        Just (Binding currentTy currentPer) -> case intersectType currentTy structTy of
          Nothing -> (Nothing, Just env) -- always false
          Just _ ->
            let truthEnv = Just (narrowEnv env var structTy)
                falsyEnv = case removeType structTy currentTy of
                  Just narrowed -> Just (narrowEnv env var narrowed)
                  Nothing -> Nothing
             in (truthEnv, falsyEnv)
        Nothing -> error ("Lookup on undefined variable " ++ varName)
branchEnv env (TCallExpression _ (TIdentifierLit _ name _) [var@(TIdentifierLit _ varName (Binding varTy per))] _) =
  case Map.lookup name assertMap of
    Just assertedTy -> do
      case lookupVar varName env of
        Just (Binding currentVarTy currentPer) -> case intersectType currentVarTy assertedTy of
          Nothing -> (Nothing, Just env) -- always false
          Just narrowed ->
            let truthEnv = Just (narrowEnv env var narrowed)
                falsyEnv = case removeType narrowed currentVarTy of
                  Just narrowedTy -> Just (narrowEnv env var narrowedTy)
                  Nothing -> Nothing
             in (truthEnv, falsyEnv)
        Nothing -> error ("Lookup on undefined variable " ++ varName)
    Nothing -> (Just env, Just env)
  where
    assertMap = Map.fromList typeCheckingBuiltInsAssert
branchEnv env _ = (Just env, Just env)

mergeBranchEnvs :: TypecheckEnv -> Maybe TypecheckEnv -> Maybe TypecheckEnv -> Maybe TypecheckEnv
mergeBranchEnvs baseEnv leftEnv rightEnv =
  case (leftEnv, rightEnv) of
    (Nothing, Nothing) -> Nothing
    (Just env, Nothing) -> Just env
    (Nothing, Just env) -> Just env
    (Just _, Just _) -> Just baseEnv

inferBlockReturnType :: [Statement] -> Check (TStatement, [Type])
inferBlockReturnType stmts = do
  typed <- mapM statementTypeChecker stmts
  let lastTy = case reverse typed of
        (TReturnStatement _ tExpr : _) -> [getType tExpr]
        (TExpressionStatement _ tExpr : _) -> [getType tExpr]
        _ -> []
      typedStmt = TBlockStatement typed
  return (typedStmt, lastTy)

narrowEnv :: TypecheckEnv -> TExpression -> Type -> TypecheckEnv
narrowEnv env expr narrowTy = case expr of
  TIdentifierLit _ name _ ->
    updateBinding name (setBindingType narrowTy) env
  TFieldAccess _ (TIdentifierLit _ objectName _) fieldName _ ->
    insertFieldRefinement objectName fieldName narrowTy env
  _ ->
    env

removeType :: Type -> Type -> Maybe Type
removeType removeTy ty =
  case ty of
    UnionT s ->
      let s' = Set.delete removeTy s
       in case Set.toList s' of
            [] -> Nothing
            [t] -> Just t
            _ -> Just (UnionT s')
    _ ->
      if ty == removeTy
        then Nothing
        else Just ty

removeNullType :: Type -> Maybe Type
removeNullType = removeType NullT

bindingNullType :: Binding -> Maybe Binding
bindingNullType (Binding ty mut) =
  fmap (\newTy -> Binding newTy mut) (removeNullType ty)

--
--
normalizeUnion :: Type -> Maybe Type
normalizeUnion (UnionT set)
  | Set.null set = Nothing
  | otherwise = Just (UnionT set)
normalizeUnion ty = Just ty

truthyType :: Type -> Maybe Type
truthyType (UnionT set) = normalizeUnion (UnionT (Set.filter canBeTruthy set))
truthyType VoidT = Nothing
truthyType NullT = Nothing
truthyType ty = Just ty

falsyType :: Type -> Maybe Type
falsyType (UnionT set) = normalizeUnion (UnionT (Set.filter canBeFalsy set))
falsyType ty =
  if canBeFalsy ty
    then Just ty
    else Nothing

data BranchNode
  = BranchNode
      { nodeStmt :: TStatement,
        nodeEnv :: TypecheckEnv,
        nodeType :: NodeTypes
      }
  | Unreachable
  deriving (Eq, Show)

data NodeTypes
  = Expr Type
  | Ret Type
  | Void
  deriving (Eq, Show)

nodeFromData :: Maybe TypecheckEnv -> [Statement] -> Check BranchNode
nodeFromData Nothing _ = pure Unreachable
nodeFromData (Just env) stmts = do
  put env
  tBlock <- statementTypeChecker (BlockStatement stmts)
  newEnv <- get
  case tBlock of
    TBlockStatement typed -> do
      let nodeTy = case blockExprInfo typed of
            (Just VoidT, Nothing) -> Void
            (Just t1, Nothing) -> Expr t1
            (Nothing, Just t2) -> Ret t2
      pure (BranchNode tBlock newEnv nodeTy)
    _ -> lift $ Left $ InternalError {message = "Expected block statement", pos = Nothing}

blockExprInfo :: [TStatement] -> (Maybe Type, Maybe Type)
blockExprInfo typed =
  case reverse typed of
    (TReturnStatement _ tExpr : _) -> (Nothing, (Just $ getType tExpr))
    (TExpressionStatement _ tExpr : _) -> ((Just $ getType tExpr), Nothing)
    _ -> (Just $ VoidT, Nothing)

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

toSourceCodeTypeChecker :: String -> TypeError -> String
toSourceCodeTypeChecker contents err =
  let ls = lines contents
   in case getMaybePos err of
        Just (Position l c _) ->
          if l > 0 && l <= length ls
            then interpolate c (ls !! (l - 1))
            else "Invalid line number"
        Nothing ->
          "No position info"
  where
    interpolate :: Int -> String -> String
    interpolate col str
      | null str = "~~"
      | col <= 0 = "~" ++ str
      | col > length str = str ++ "~"
      | otherwise =
          let i = col - 1
           in take i str ++ "~" ++ [str !! i] ++ "~" ++ drop (i + 1) str
