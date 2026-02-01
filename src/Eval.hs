module Eval where

import Ast

type Enviroment = ()

type Object = ()

evalStatement :: Statement -> Enviroment -> Object
evalStatement = undefined

evalExpression :: Expression -> Enviroment -> Enviroment
evalExpression = undefined
