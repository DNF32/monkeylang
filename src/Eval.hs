module Eval where

import Ast (Expression (..), Statement (..))
import Control.Monad.State
import Data.Map qualified as Map

emptyMap :: Map.Map k v
emptyMap = Map.empty

data Object
  = IntO Integer
  | BoolO Bool
  | FloatO Float
  | StringO String
  | ArrayO [Object]
  deriving (Show, Eq)

type Enviroment = Map.Map String Object

type Eval a = State Enviroment a

hasIdent :: String -> Eval Bool
hasIdent name = gets (Map.member name)

setObject :: String -> Object -> Eval ()
setObject name obj = modify (Map.insert name obj)

getObject :: String -> Eval (Maybe Object)
getObject name = gets (Map.lookup name)

newEnv :: Enviroment
newEnv = Map.empty

initialEnv :: Eval ()
initialEnv = put newEnv

evalLetStatement :: Statement -> Eval Object
evalLetStatement (LetStatement tok (IdentifierLit _ name) value) = do
  obj <- evalExpression value
  setObject name obj
  return obj
evalLetStatement _ = undefined

evalReturnStatement :: Statement -> Eval Object
evalReturnStatement (ReturnStatement tok expr) = do
  evalExpression expr

evalExpressionStatement :: Statement -> Eval Object
evalExpressionStatement = undefined

evalExpression :: Expression -> Eval Object
evalExpression = undefined
