{-# LANGUAGE DuplicateRecordFields #-}
module Ast where
import           Token (Token)

-- Identifier is its own type (like in Go)
data Identifier = Identifier
                    { token :: Token
                    , name  :: String
                    }
  deriving (Eq, Show)

data Statement = Program
                   { statements :: [Statement]
                   }
               | LetStatement
                   { token :: Token
                   , name  :: Identifier
                   , value :: Expression
                   }
               | ReturnStatement
                   { token  :: Token
                   , result :: Expression
                   }
               | ExpressionStatement
                   { token :: Token
                   , expr  :: Expression
                   }
               | BlockStatement
                   { statements :: [Statement]
                   }
               | IfStatement
                   { token       :: Token
                   , condition   :: Expression
                   , consequence :: [Statement]
                   , alternative :: Maybe [Statement]
                   }
  deriving (Eq, Show)

data Operator = Plus | Minus | Multiply | Divide | LT | GT | Equals | NotEquals | And | Or | Bang
  deriving (Eq, Ord, Show)

data Expression = IntLiteral
                    { token    :: Token
                    , intValue :: Integer
                    }
                | StringLiteral
                    { token       :: Token
                    , stringValue :: String
                    }
                | IdentifierExpr Identifier
                | PrefixExpression
                    { token    :: Token
                    , operator :: Operator
                    , right    :: Expression
                    }
                | InfixExpression
                    { token    :: Token
                    , left     :: Expression
                    , operator :: Operator
                    , right    :: Expression
                    }
                | FunctionLiteral
                    { token      :: Token
                    , parameters :: [Identifier]
                    , body       :: [Statement]
                    }
                | CallExpression
                    { token     :: Token
                    , function  :: Expression
                    , arguments :: [Expression]
                    }
                | IndexExpression
                    { token :: Token
                    , left  :: Expression
                    , index :: Expression
                    }
                | ArrayLiteral
                    { token    :: Token
                    , elements :: [Expression]
                    }
  deriving (Eq, Show)

