{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast
  ( Expression (..),
    Statement (..),
    (?==),
    Precedence (..),
    expressionToString,
    statementToString,
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
import Data.List (intercalate)
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

infix 4 ?==

(?==) :: Expression -> Expression -> Bool
IntLit {intValue = lhs} ?== IntLit {intValue = rhs} = lhs == rhs
FloatLit {floatValue = lhs} ?== FloatLit {floatValue = rhs} = lhs == rhs
StringLit {stringValue = lhs} ?== StringLit {stringValue = rhs} = lhs == rhs
IdentifierLit {name = lhs} ?== IdentifierLit {name = rhs} = lhs == rhs
ArrayLit {elements = lhs} ?== ArrayLit {elements = rhs} = expressionsEqual lhs rhs
BooleanLit {value = lhs} ?== BooleanLit {value = rhs} = lhs == rhs
PrefixExpression {operator = lop, right = lr} ?== PrefixExpression {operator = rop, right = rr} =
  lop == rop && lr ?== rr
InfixExpression {left = ll, operator = lop, right = lr} ?== InfixExpression {left = rl, operator = rop, right = rr} =
  lop == rop && ll ?== rl && lr ?== rr
FunctionLit {parameters = lp, body = lb} ?== FunctionLit {parameters = rp, body = rb} =
  expressionsEqual lp rp && statementsEqual lb rb
CallExpression {function = lf, arguments = la} ?== CallExpression {function = rf, arguments = ra} =
  lf ?== rf && expressionsEqual la ra
IndexExpression {left = ll, index = li} ?== IndexExpression {left = rl, index = ri} =
  ll ?== rl && li ?== ri
IfExpression {condition = lc, consequence = lcons, alternative = lalt}
  ?== IfExpression {condition = rc, consequence = rcons, alternative = ralt} =
    lc ?== rc && statementsEqual lcons rcons && maybeStatementsEqual lalt ralt
_ ?== _ = False

data Statement
  = Program {statements :: [Statement]}
  | LetStatement {token :: Token, name :: Expression, value :: Expression} -- Expression!
  | ReturnStatement {token :: Token, result :: Expression}
  | ExpressionStatement {token :: Token, expr :: Expression}
  | BlockStatement {statements :: [Statement]}
  deriving (Eq, Show)

expressionsEqual :: [Expression] -> [Expression] -> Bool
expressionsEqual [] [] = True
expressionsEqual (lhs : restLhs) (rhs : restRhs) =
  lhs ?== rhs && expressionsEqual restLhs restRhs
expressionsEqual _ _ = False

statementsEqual :: [Statement] -> [Statement] -> Bool
statementsEqual [] [] = True
statementsEqual (lhs : restLhs) (rhs : restRhs) =
  statementEqual lhs rhs && statementsEqual restLhs restRhs
statementsEqual _ _ = False

statementEqual :: Statement -> Statement -> Bool
statementEqual (Program {statements = lhs}) (Program {statements = rhs}) =
  statementsEqual lhs rhs
statementEqual LetStatement {name = lhsName, value = lhsValue} LetStatement {name = rhsName, value = rhsValue} =
  lhsName ?== rhsName && lhsValue ?== rhsValue
statementEqual ReturnStatement {result = lhsResult} ReturnStatement {result = rhsResult} =
  lhsResult ?== rhsResult
statementEqual ExpressionStatement {expr = lhsExpr} ExpressionStatement {expr = rhsExpr} =
  lhsExpr ?== rhsExpr
statementEqual BlockStatement {statements = lhs} BlockStatement {statements = rhs} =
  statementsEqual lhs rhs
statementEqual _ _ = False

maybeStatementsEqual :: Maybe [Statement] -> Maybe [Statement] -> Bool
maybeStatementsEqual Nothing Nothing = True
maybeStatementsEqual (Just lhs) (Just rhs) = statementsEqual lhs rhs
maybeStatementsEqual _ _ = False

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

data Precedence = LOWEST | EQUALS | LESSGREATER | SUM | PRODUCT | PREFIX | CALL | INDEX deriving (Ord, Eq, Show)

infixToPrecedence :: TokenType -> Precedence
infixToPrecedence token = case token of
  Equal -> EQUALS
  NotEqual -> EQUALS
  LessThan -> LESSGREATER
  GreaterThan -> LESSGREATER
  Plus -> SUM
  Minus -> SUM
  Slash -> PRODUCT
  Asterisk -> PRODUCT
  LParen -> CALL
  LBracket -> INDEX
  otherwise -> LOWEST

statementToString :: Statement -> String
statementToString (LetStatement _ name value) = "let " ++ expressionToString name ++ " = " ++ expressionToString value
statementToString (ReturnStatement _ value) = "return " ++ expressionToString value
statementToString (ExpressionStatement _ value) = expressionToString value
statementToString (BlockStatement stmts) = intercalate "" (map statementToString stmts)
statementToString (Program stmts) = intercalate "" (map statementToString stmts)

expressionToString :: Expression -> String
expressionToString (IntLit _ val) = show val
expressionToString (FloatLit _ val) = show val
expressionToString (StringLit _ val) = val
expressionToString (IdentifierLit _ val) = val
expressionToString (BooleanLit _ val) = show val
expressionToString (ArrayLit _ elements) = "[" ++ intercalate ", " (map expressionToString elements) ++ "]"
expressionToString (PrefixExpression _ Bang val) = "(" ++ "!" ++ expressionToString val ++ ")"
expressionToString (PrefixExpression _ Minus val) = "(" ++ "-" ++ expressionToString val ++ ")"
expressionToString (InfixExpression _ left Plus right) = "(" ++ expressionToString left ++ " + " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Minus right) = "(" ++ expressionToString left ++ " - " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Asterisk right) = "(" ++ expressionToString left ++ " * " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Slash right) = "(" ++ expressionToString left ++ " / " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left LessThan right) = "(" ++ expressionToString left ++ " < " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left GreaterThan right) = "(" ++ expressionToString left ++ " > " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Equal right) = "(" ++ expressionToString left ++ " == " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left NotEqual right) = "(" ++ expressionToString left ++ " != " ++ expressionToString right ++ ")"
