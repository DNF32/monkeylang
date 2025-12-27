module Token where

data TokenType
  = Illegal
  | EOF
  | Identifier String
  | IntLiteral Int
  | StringLiteral String
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | Comma
  | Semicolon
  | Colon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Function
  | Let
  | TrueLit
  | FalseLit
  | If
  | Else
  | Return
  deriving (Show, Eq)

data Token = Token
  { tokenType :: TokenType,
    tokenPosition :: Position
  }
  deriving (Show, Eq)

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

data LexError = LexError
  { errorMsg :: String,
    errorPosition :: Position
  }

data LexerState = LexerState
  { getInput :: String,
    currentPosition :: Position
  }

nextToken :: Lexer a -> LexerState -> Either LexError (Token, LexerState)
nextToken = undefined

newtype Lexer a = Lexer
  {runLexer :: LexerState -> Either LexError (a, LexerState)}

-- the final Lexer will be some kind Lexer [Token]
