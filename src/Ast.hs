{-# LANGUAGE DuplicateRecordFields #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast where

import Token (Token, TokenType (..), tokenType)

data IdentifierNode = IdentifierNode
  { token :: Token,
    name :: String
  }
  deriving (Eq, Show)

data Statement
  = Program
      { statements :: [Statement]
      }
  | LetStatement
      { token :: Token,
        name :: IdentifierNode,
        value :: Expression
      }
  | ReturnStatement
      { token :: Token,
        result :: Expression
      }
  | ExpressionStatement
      { token :: Token,
        expr :: Expression
      }
  | BlockStatement
      { statements :: [Statement]
      }
  | IfStatement
      { token :: Token,
        condition :: Expression,
        consequence :: [Statement],
        alternative :: Maybe [Statement]
      }
  deriving (Eq, Show)

type Operator = String

data Expression
  = IntLit
      { token :: Token,
        intValue :: Integer
      }
  | FloatLit
      { token :: Token,
        floatValue :: Float
      }
  | StringLit
      { token :: Token,
        stringValue :: String
      }
  | IdentifierExpr IdentifierNode
  | PrefixExpression
      { token :: Token,
        operator :: Operator,
        right :: Expression
      }
  | InfixExpression
      { token :: Token,
        left :: Expression,
        operator :: Operator,
        right :: Expression
      }
  | FunctionLit
      { token :: Token,
        parameters :: [IdentifierNode],
        body :: [Statement]
      }
  | CallExpression
      { token :: Token,
        function :: Expression,
        arguments :: [Expression]
      }
  | IndexExpression
      { token :: Token,
        left :: Expression,
        index :: Expression
      }
  | ArrayLit
      { token :: Token,
        elements :: [Expression]
      }
  deriving (Eq, Show)

infixPrecedence :: Token -> Integer
infixPrecedence token = case tokenType token of
  Equal _ -> 1
  NotEqual _ -> 10
  LessThan _ -> 10
  GreaterThan _ -> 10
  Plus _ -> 10
  Minus _ -> 10
  Slash _ -> 10
  Asterisk _ -> 10
  LParen _ -> 10
  LBracket _ -> 10
  otherwise -> 0

-------------------------
