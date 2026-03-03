module TypeChecker where

import Data.Map qualified as Map

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
  | StructT String (Map.Map String Type) -- name + fields
  | AnyT
  deriving (Show, Eq)

type StructDef = Map.Map String Type -- field name → type

type TypeDefs = Map.Map String StructDef -- struct name → fields

type TypeEnv = Map.Map String Type -- Like the enviroment, will be used to keep track of types of stuff
