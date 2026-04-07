module Types where

import Data.List (nub)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Token (Position)

-- ============================================================
-- Types
-- ============================================================
data Type
  = IntT
  | BoolT
  | FloatT
  | StringT
  | NullT
  | VoidT
  | ArrayT Type
  | FnT [Type] Type
  | UnionT (Set.Set Type)
  | StructT StructName
  | UnresolvedT StructName
  | AnyT
  deriving (Show, Eq, Ord)

simplifyRetTy :: [Type] -> Type
simplifyRetTy ts =
  let s = Set.fromList ts
   in case Set.toList s of
        [] -> VoidT
        [t] -> t
        _ -> UnionT s

-- TODO: Need to write a test for this cases of inclusion
isCompatible :: Type -> Type -> Bool
isCompatible inferred annotated =
  case (inferred, annotated) of
    (AnyT, _) -> True -- inferred is AnyT, accepts any annotation
    (_, AnyT) -> False -- inferred is specific, doesn't match AnyT annotation
    (UnionT setInferred, UnionT setAnnotated) ->
      Set.isSubsetOf setAnnotated setInferred
    (UnionT setInferred, _) ->
      Set.member annotated setInferred
    (t1, t2) -> t1 == t2 -- otherwise exact match

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

type TypeEnv = Map.Map String Type

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
  deriving (Show, Eq)

withPos :: Position -> TypeError -> TypeError
withPos p err = err {pos = Just p}

-- ============================================================
-- Environment
-- ============================================================

data TypecheckEnv = TypecheckEnv
  { typeEnv :: TypeEnv,
    typeDefs :: TypeDefs,
    currentRetTy :: [Type]
  }
  deriving (Show)

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
emptyEnv = TypecheckEnv Map.empty Map.empty []

lookupVar :: String -> TypecheckEnv -> Maybe Type
lookupVar name env = Map.lookup name (typeEnv env)

lookupStruct :: StructName -> TypecheckEnv -> Maybe StructDef
lookupStruct name env = Map.lookup name (typeDefs env)

lookupFieldType :: StructName -> FieldName -> TypecheckEnv -> Maybe Type
lookupFieldType structName fieldName env = do
  fieldTypes <- lookupStruct structName env
  Map.lookup fieldName fieldTypes

insertVar :: String -> Type -> TypecheckEnv -> TypecheckEnv
insertVar name t env = env {typeEnv = Map.insert name t (typeEnv env)}

data Rule

narrow :: TypecheckEnv -> Rule -> TypecheckEnv
narrow = undefined
