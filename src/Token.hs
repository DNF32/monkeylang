module Token where

import           Control.Applicative (Alternative (..))
import           Data.Traversable    ()

data TokenType = Illegal
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
  deriving (Eq, Show)

data Token = Token
               { tokenType     :: TokenType
               , tokenPosition :: Position
               }
  deriving (Eq, Show)

data Position = Position
                  { line   :: Int
                  , column :: Int
                  }
  deriving (Eq, Show)

data LexError = LexError
                  { errorMsg      :: String
                  , errorPosition :: Position
                  }
  deriving (Show)

data LexerState = LexerState
                    { getInput        :: String
                    , currentPosition :: Position
                    }
  deriving (Show)

newLexError :: String -> Position -> LexError
newLexError m p = LexError {errorMsg = m, errorPosition = p}

nextToken :: Lexer a -> LexerState -> Either LexError (Token, LexerState)
nextToken = undefined

newtype Lexer a = Lexer { runLexer :: LexerState -> Either LexError (a, LexerState) }

------------------
-- The laws our Lexer follows
instance Functor Lexer where
  fmap :: (a -> b) -> Lexer a -> Lexer b
  fmap f la = Lexer $ \state -> case runLexer la state of
    Right
      ( a,
        newState
        ) -> Right (f a, newState)
    Left errorMsg -> Left errorMsg

instance Applicative Lexer where
  pure :: a -> Lexer a
  pure a = Lexer $ \state -> Right (a, state)
  (<*>) :: Lexer (a -> b) -> Lexer a -> Lexer b
  lab <*> la = Lexer $ \state -> case runLexer lab state of
    Right
      ( ab,
        newState
        ) -> runLexer (fmap ab la) newState
    Left errorMsg -> Left errorMsg

instance Monad Lexer where
  (>>=) :: Lexer a -> (a -> Lexer b) -> Lexer b
  la >>= alb = Lexer $ \state -> case runLexer la state of
    Left errorMsg -> Left errorMsg
    Right
      ( a,
        newState
        ) -> runLexer (alb a) newState

instance Alternative Lexer where
  empty = emptyLexer
  (<|>) :: Lexer a -> Lexer a -> Lexer a
  la <|> la2 = Lexer $ \state -> case runLexer la state of
    Left _      -> runLexer la2 state
    Right lexed -> Right lexed

--  A basic char lexer
--
emptyLexer :: Lexer a
emptyLexer = Lexer $ \state -> Left (newLexError "empty Parser failed to parse" (currentPosition state))

anyNonWhitespaceLexer :: Lexer Char
anyNonWhitespaceLexer = satisfy (`notElem` ['\n', '\r', '\t', ' '])


charL :: Char -> Lexer Char
charL s = satisfy (== s)

satisfy :: (Char -> Bool) -> Lexer Char
satisfy cb = Lexer $ \state ->
  case getInput state of
    x : xs
      | cb x ->
          let newPos = advancePosition x (currentPosition state)
           in Right (x, state {getInput = xs, currentPosition = newPos})
      | otherwise ->
          Left $ newLexError ("Failed to parse char " ++ show x) (currentPosition state)
    [] ->
      Left $ newLexError "Failed to lexer: input was empty" (currentPosition state)

oneOrMore :: Lexer a -> Lexer [a]
oneOrMore la = (:) <$> la <*> zeroOrMore la

zeroOrMore :: Lexer a -> Lexer [a]
zeroOrMore la = oneOrMore la <|> pure []

advancePosition :: Char -> Position -> Position
advancePosition c pos = case c of
  '\n' -> Position {line = line pos + 1, column = 1} -- new line
  '\r' -> pos -- ignore carriage return
  '\t' -> pos {column = column pos + tabWidth} -- tab
  _    -> pos {column = column pos + 1} -- normal char
  where
    tabWidth = 4 -- configurable

nl :: Lexer Char
ws :: Lexer Char
cr :: Lexer Char
tab :: Lexer Char
nl = charL '\n'

ws = charL ' '

cr = charL '\r'

tab = charL '\t'
