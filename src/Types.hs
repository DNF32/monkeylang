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
  | UnionT [Type]
  | StructT StructName
  | UnresolvedT StructName
  | AnyT
  deriving (Show, Eq, Ord)

simplifyUnion :: [Type] -> Type
simplifyUnion ts =
  let unique = nub ts -- remove duplicates
   in case unique of
        [t] -> t -- all returns same type, no need for union
        ts -> UnionT ts -- different types, make union

sameUnion :: Type -> Type -> Bool
sameUnion (UnionT t1) (UnionT t2) = (Set.fromList t1) == (Set.fromList t2)
sameUnion t1 t2 = t1 == t2

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
  | UndefinedVariable {varName :: String, pos :: Maybe Position}
  | NotCallable {gotType :: Type, pos :: Maybe Position}
  | InvalidParam {pos :: Maybe Position}
  | OperatorNotDefined {operator :: String, leftType :: Type, rightType :: Type, pos :: Maybe Position}
  | InternalError {message :: String, pos :: Maybe Position}
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

insertVar :: String -> Type -> TypecheckEnv -> TypecheckEnv
insertVar name t env = env {typeEnv = Map.insert name t (typeEnv env)}
