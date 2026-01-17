{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast
  ( Expression (..),
    Statement (..),
    infixToPrecedence,
    -- Expression lenses
    exprToken,
    exprIntValue,
    exprFloatValue,
    exprStringValue,
    exprName,
    exprBoolValue,
    exprOperator,
    exprRight,
    exprLeft,
    exprParameters,
    exprBody,
    exprFunction,
    exprArguments,
    exprIndex,
    exprElements,
    -- Statement lenses
    stmtToken,
    stmtName,
    stmtValue,
    stmtResult,
    stmtExpr,
    stmtStatements,
  )
where

import Control.Lens
import Token (LexerState (currentPosition), Token (..), TokenType (..), tokenType)

type Operator = TokenType

data Expression
  = IntLit {token :: Token, intValue :: Integer}
  | FloatLit {token :: Token, floatValue :: Float}
  | StringLit {token :: Token, stringValue :: String}
  | IdentifierLit {token :: Token, name :: String}
  | ArrayLit {token :: Token, elements :: [Expression]}
  | BooleanLit {token :: Token, value :: Bool}
  | PrefixExpression {token :: Token, operator :: Operator, right :: Expression}
  | InfixExpression {token :: Token, left :: Expression, operator :: Operator, right :: Expression}
  | FunctionLit {token :: Token, parameters :: [Expression], body :: [Statement]} -- Expression!
  | CallExpression {token :: Token, function :: Expression, arguments :: [Expression]}
  | IndexExpression {token :: Token, left :: Expression, index :: Expression}
  | IfExpression {token :: Token, condition :: Expression, consequence :: [Statement], alternative :: Maybe [Statement]}
  deriving (Eq, Show)

-- (==?) :: Expression -> Expression -> Bool
-- IntLit (Token {tokenPosition = posa}) a
--  ==? IntLit (Token {tokenPosition = posb}) b =
--    a == b && posa == posb
-- FloatLit (Token {tokenPosition = posa}) a
--  ==? FloatLit (Token {tokenPosition = posb}) b =
--    a == b && posa == posb
-- StringLit (Token {tokenPosition = posa}) a
--  ==? StringLit (Token {tokenPosition = posb}) b =
--    a == b && posa == posb
-- IdentifierLit (Token {tokenPosition = posa}) a
--  ==? IdentifierLit (Token {tokenPosition = posb}) b =
--    a == b && posa == posb
-- BooleanLit (Token {tokenPosition = posa}) a
--  ==? BooleanLit (Token {tokenPosition = posb}) b =
--    a == b && posa == posb
-- PrefixExpression (Token {tokenPosition = posa}) op1 r1
--  ==? PrefixExpression (Token {tokenPosition = posb}) op2 r2 =
--    op1 == op2 && r1 ==? r2 && posa == posb
-- InfixExpression (Token {tokenPosition = posa}) l1 op1 r1
--  ==? InfixExpression (Token {tokenPosition = posb}) l2 op2 r2 =
--    op1 == op2 && l1 ==? l2 && r1 ==? r2 && posa == posb

data Statement
  = Program {statements :: [Statement]}
  | LetStatement {token :: Token, name :: Expression, value :: Expression} -- Expression!
  | ReturnStatement {token :: Token, result :: Expression}
  | ExpressionStatement {token :: Token, expr :: Expression}
  | BlockStatement {statements :: [Statement]}
  deriving (Eq, Show)

makeLensesFor
  [ ("token", "exprToken"),
    ("intValue", "exprIntValue"),
    ("floatValue", "exprFloatValue"),
    ("stringValue", "exprStringValue"),
    ("name", "exprName"),
    ("value", "exprBoolValue"), -- BooleanLit only
    ("operator", "exprOperator"),
    ("right", "exprRight"),
    ("left", "exprLeft"),
    ("parameters", "exprParameters"),
    ("body", "exprBody"),
    ("function", "exprFunction"),
    ("arguments", "exprArguments"),
    ("index", "exprIndex"),
    ("elements", "exprElements")
  ]
  ''Expression

-- Statement lenses
makeLensesFor
  [ ("token", "stmtToken"),
    ("name", "stmtName"),
    ("value", "stmtValue"),
    ("result", "stmtResult"),
    ("expr", "stmtExpr"),
    ("statements", "stmtStatements")
  ]
  ''Statement

infixToPrecedence :: TokenType -> Integer
infixToPrecedence token = case token of
  Equal -> 1
  NotEqual -> 10
  LessThan -> 10
  GreaterThan -> 10
  Plus -> 10
  Minus -> 10
  Slash -> 15
  Asterisk -> 15
  LParen -> 10
  LBracket -> 10
  otherwise -> 0
