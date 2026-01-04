{-# LANGUAGE DuplicateRecordFields #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast where

import Token (Token, TokenType (..), tokenType)

type Operator = String

data Expression
  = IntLit {token :: Token, intValue :: Integer}
  | FloatLit {token :: Token, floatValue :: Float}
  | StringLit {token :: Token, stringValue :: String}
  | IdentifierLit {token :: Token, name :: String}
  | PrefixExpression {token :: Token, operator :: Operator, right :: Expression}
  | InfixExpression {token :: Token, left :: Expression, operator :: Operator, right :: Expression}
  | FunctionLit {token :: Token, parameters :: [Expression], body :: [Statement]} -- Expression!
  | CallExpression {token :: Token, function :: Expression, arguments :: [Expression]}
  | IndexExpression {token :: Token, left :: Expression, index :: Expression}
  | ArrayLit {token :: Token, elements :: [Expression]}
  deriving (Eq, Show)

data Statement
  = Program {statements :: [Statement]}
  | LetStatement {token :: Token, name :: Expression, value :: Expression} -- Expression!
  | ReturnStatement {token :: Token, result :: Expression}
  | ExpressionStatement {token :: Token, expr :: Expression}
  | BlockStatement {statements :: [Statement]}
  | IfStatement {token :: Token, condition :: Expression, consequence :: [Statement], alternative :: Maybe [Statement]}
  deriving (Eq, Show)

infixPrecedence :: Token -> Integer
infixPrecedence token = case tokenType token of
  Equal -> 1
  NotEqual -> 10
  LessThan -> 10
  GreaterThan -> 10
  Plus -> 10
  Minus -> 10
  Slash -> 10
  Asterisk -> 10
  LParen -> 10
  LBracket -> 10
  otherwise -> 0

-------------------------
