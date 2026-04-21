{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.List (nub)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Token (HasPos (..), Position (..), Token)

-- ============================================================
-- Types
-- ============================================================
data Type
  = IntT
  | BoolT
  | FloatT
  | StringT
  | NullT
  | NeverT
  | VoidT
  | ArrayT Type
  | FnT [Binding] Type
  | UnionT (Set.Set Type)
  | StructT StructName
  | UnresolvedT StructName
  | AnyT
  deriving (Show, Eq, Ord)

newtype RetTypes = RetTypes {unRetTypes :: [Type]}
  deriving (Show, Eq)

instance Semigroup RetTypes where
  RetTypes a <> RetTypes b = RetTypes (a <> b)

instance Monoid RetTypes where
  mempty = RetTypes []

simplifyRetTy :: [Type] -> Type
simplifyRetTy ts =
  let mergedSet = foldr mergeTypes Set.empty ts
   in case Set.toList mergedSet of
        [] -> VoidT
        [t] -> t
        _ -> UnionT mergedSet
  where
    mergeTypes :: Type -> Set.Set Type -> Set.Set Type
    mergeTypes (UnionT inner) st = Set.union inner st
    mergeTypes st ty = Set.insert st ty

-- TODO: Need to write a test for this cases of inclusion
isCompatible :: Type -> Type -> Bool
isCompatible inferred annotated =
  case (inferred, annotated) of
    (UnionT setInferred, UnionT setAnnotated) ->
      Set.isSubsetOf setInferred setAnnotated
    (t, UnionT setAnnotated) ->
      Set.member t setAnnotated
    (UnionT setInferred, t) ->
      Set.member t setInferred && Set.size setInferred == 1
    (AnyT, _) -> True
    (t1, t2) -> t1 == t2 -- otherwise exact match

intersect :: Type -> Type -> Bool
intersect t1 t2 =
  case (t1, t2) of
    (UnionT t1, UnionT t2) ->
      not . Set.null $ Set.intersection t1 t2
    (UnionT t1, t2) ->
      Set.member t2 t1
    (t1, UnionT t2) ->
      Set.member t1 t2
    (t1, t2) -> t1 == t2

intersectType :: Type -> Type -> Maybe Type
intersectType t1 t2 =
  case (t1, t2) of
    (UnionT s1, UnionT s2) ->
      let s = Set.intersection s1 s2
       in if Set.null s then Nothing else Just (UnionT s)
    (UnionT s, t) ->
      if Set.member t s then Just t else Nothing
    (t, UnionT s) ->
      if Set.member t s then Just t else Nothing
    (t1', t2') ->
      if t1' == t2' then Just t1' else Nothing

subtractType :: Type -> Type -> Maybe Type
subtractType t1 t2 =
  case (t1, t2) of
    (UnionT s1, UnionT s2) ->
      let s = Set.difference s1 s2
       in if Set.null s then Nothing else Just (UnionT s)
    (UnionT s, t) ->
      let s' = Set.delete t s
       in if Set.null s' then Nothing else Just (UnionT s')
    (t, UnionT s) ->
      if Set.member t s then Nothing else Just t
    (t1', t2') ->
      if t1' == t2' then Nothing else Just t1'

diffTypes :: Type -> Type -> (Maybe Type, Maybe Type)
diffTypes t1 t2 =
  (subtractType t1 t2, subtractType t2 t1)

-- Check if a type is optional (can be Null)
isOptional :: Type -> Bool
isOptional NullT = True
isOptional (UnionT s) = Set.member NullT s
isOptional _ = False

-- Check if a type is contained in a union
unionHas :: Type -> Type -> Bool
unionHas ty (UnionT set) = Set.member ty set
unionHas _ _ = False

type StructName = String

type FieldName = String

type StructDef = Map.Map FieldName Type

type TypeDefs = Map.Map StructName StructDef

type TypeEnv = Map.Map String Binding

type FuncEnv = Map.Map String Type

class HasType a where
  getType :: a -> Type

data Binding = Binding
  { bindTy :: Type,
    bindMutable :: Mutability
  }
  deriving (Eq, Show, Ord)

instance HasType Binding where
  getType (Binding ty _) = ty

data Mutability = Immutable | Mutable
  deriving (Eq, Show, Ord)

mapBindingType :: (Type -> Type) -> Binding -> Binding
mapBindingType f b = b {bindTy = f (bindTy b)}

setBindingType :: Type -> Binding -> Binding
setBindingType ty b = b {bindTy = ty}

-- ============================================================
-- Errors
-- ============================================================

data TypeError
  = TypeMismatch {expected :: Type, got :: Type, pos :: Maybe Position}
  | UndefinedVariable {varName :: String, pos :: Maybe Position, localMessage :: Maybe String}
  | NotCallable {gotType :: Type, pos :: Maybe Position}
  | InvalidParam {pos :: Maybe Position}
  | OperatorNotDefined {operator :: String, leftType :: Type, rightType :: Type, pos :: Maybe Position}
  | InternalError {message :: String, pos :: Maybe Position}
  | UndefinedStruct {undefinedName :: String, pos :: Maybe Position}
  | UndefinedField {structName :: String, undefinedFieldName :: String, pos :: Maybe Position}
  | UnreachableNode {message :: String, pos :: Maybe Position}
  | MissMatchOnCallable {errors :: [BindingError Token]}
  deriving (Show, Eq)

instance HasPos TypeError where
  getPos err =
    case getMaybePos err of
      Just p -> p
      Nothing -> Position 0 0 0

  getMaybePos (TypeMismatch _ _ p) = p
  getMaybePos (UndefinedVariable _ p _) = p
  getMaybePos (NotCallable _ p) = p
  getMaybePos (InvalidParam p) = p
  getMaybePos (OperatorNotDefined _ _ _ p) = p
  getMaybePos (InternalError _ p) = p
  getMaybePos (UndefinedStruct _ p) = p
  getMaybePos (UndefinedField _ _ p) = p
  getMaybePos (UnreachableNode _ p) = p
  getMaybePos (MissMatchOnCallable _) = Nothing

data MatchResult
  = TypeIncompatible
  | MutabilityIncompatible
  | IncompatibleBoth
  | MatchSuccess
  deriving (Eq, Show)

data BindingError a = BindingError {err :: MatchResult, value :: a}
  deriving (Show, Eq)

withPos :: Position -> TypeError -> TypeError
withPos p err = err {pos = Just p}

-- ============================================================
-- Environment
-- ============================================================

data TypecheckEnv = TypecheckEnv
  { typeEnv :: [TypeEnv],
    funcEnv :: [FuncEnv],
    typeDefs :: TypeDefs,
    currentRetTy :: [Type],
    fieldEnv :: Map.Map (String, String) Type
  }
  deriving (Eq, Show)

pushRetTy :: Type -> TypecheckEnv -> TypecheckEnv
pushRetTy ty env =
  env {currentRetTy = ty : currentRetTy env}

popRetTy :: TypecheckEnv -> TypecheckEnv
popRetTy env =
  env {currentRetTy = tail (currentRetTy env)}

peekRetTy :: TypecheckEnv -> Maybe Type
peekRetTy env =
  case currentRetTy env of
    [] -> Nothing
    (t : _) -> Just t

emptyEnv :: TypecheckEnv
emptyEnv = TypecheckEnv [] [Map.fromList typeCheckingBuiltIns] Map.empty [] Map.empty

lookupVar :: String -> TypecheckEnv -> Maybe Binding
lookupVar name env = go (typeEnv env)
  where
    go [] = Nothing
    go (scope : rest) =
      case Map.lookup name scope of
        Just ty -> Just ty
        Nothing -> go rest

lookupStruct :: StructName -> TypecheckEnv -> Maybe StructDef
lookupStruct name env = Map.lookup name (typeDefs env)

lookupFieldType :: StructName -> FieldName -> TypecheckEnv -> Maybe Type
lookupFieldType structName fieldName env = do
  fieldTypes <- lookupStruct structName env
  Map.lookup fieldName fieldTypes

lookupFieldRefinement :: String -> String -> TypecheckEnv -> Maybe Type
lookupFieldRefinement var field env =
  Map.lookup (var, field) (fieldEnv env)

insertFieldRefinement :: String -> String -> Type -> TypecheckEnv -> TypecheckEnv
insertFieldRefinement var field ty env =
  env {fieldEnv = Map.insert (var, field) ty (fieldEnv env)}

clearFieldRefinements :: String -> TypecheckEnv -> TypecheckEnv
clearFieldRefinements var env =
  env {fieldEnv = Map.filterWithKey (\(v, _) _ -> v /= var) (fieldEnv env)}

updateVar :: String -> Type -> TypecheckEnv -> TypecheckEnv
updateVar name t env = env {typeEnv = go (typeEnv env)}
  where
    go [] = error ("updateVar: variable not found: " ++ name)
    go (scope : rest)
      | Map.member name scope = Map.adjust (\(Binding _ per) -> (Binding t per)) name scope : rest
      | otherwise = scope : go rest

defineBinding :: String -> Binding -> TypecheckEnv -> TypecheckEnv
defineBinding name bd env =
  case typeEnv env of
    [] -> env {typeEnv = [Map.singleton name bd]}
    (scope : rest) -> env {typeEnv = Map.insert name bd scope : rest}

updateBinding :: String -> (Binding -> Binding) -> TypecheckEnv -> TypecheckEnv
updateBinding name f env = env {typeEnv = go (typeEnv env)}
  where
    go [] = error ("updateBinding: variable not found: " ++ name)
    go (scope : rest)
      | Map.member name scope =
          Map.adjust f name scope : rest
      | otherwise =
          scope : go rest

pushScope :: TypecheckEnv -> TypecheckEnv
pushScope env = env {typeEnv = Map.empty : typeEnv env}

popScope :: TypecheckEnv -> TypecheckEnv
popScope env =
  case typeEnv env of
    [] -> env
    (_ : rest) -> env {typeEnv = rest}

data Rule

narrow :: TypecheckEnv -> Rule -> TypecheckEnv
narrow = undefined

----------Built ins
--

builtInParamType :: Binding
builtInParamType =
  let primitive = Set.fromList [IntT, StringT, BoolT, FloatT, NullT, VoidT]
      arrayType = ArrayT (UnionT primitive)
      generalType = UnionT (Set.insert arrayType primitive)
   in (Binding generalType Immutable)

typeCheckingBuiltIns :: [(String, Type)]
typeCheckingBuiltIns =
  [ ("isInt", FnT [builtInParamType] BoolT),
    ("isString", FnT [builtInParamType] BoolT),
    ("isFloat", FnT [builtInParamType] BoolT),
    ("isBool", FnT [builtInParamType] BoolT)
  ]

typeCheckingBuiltInsAssert :: [(String, Type)]
typeCheckingBuiltInsAssert =
  [ ("isInt", IntT),
    ("isString", StringT),
    ("isFloat", FloatT),
    ("isBool", BoolT)
  ]
