{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Token where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Traversable ()

-- Core types
data TokenType
  = Illegal
  | EOF
  | Identifier String
  | IntLiteral IntegerPart
  | FloatLiteral IntegerPart (Maybe FractionalPart) (Maybe ExponentPart)
  | StringLiteral String
  | Assign Char
  | Plus Char
  | Minus Char
  | Bang Char
  | Asterisk Char
  | Slash Char
  | Equal String
  | NotEqual String
  | LessThan Char
  | GreaterThan Char
  | Comma Char
  | Semicolon Char
  | Colon Char
  | LParen Char
  | RParen Char
  | LBrace Char
  | RBrace Char
  | LBracket Char
  | RBracket Char
  | Function String
  | Let String
  | TrueLit String
  | FalseLit String
  | If String
  | Else String
  | Return String
  | Escaped Char
  deriving (Eq, Show)

data Token = Token
  { tokenType :: TokenType,
    tokenPosition :: Position
  }
  deriving (Eq, Show)

data Number
  = IntNum IntegerPart
  | FloatNum IntegerPart (Maybe FractionalPart) (Maybe ExponentPart)
  deriving (Eq, Show)

-- Wrappers for the components
newtype IntegerPart = IntegerPart
  {intDigits :: String}
  deriving (Eq, Show)

newtype FractionalPart = FractionalPart
  {fracDigits :: String}
  deriving (Eq, Show)

data ExponentPart = ExponentPart
  { expSign :: Maybe Char, -- '+' or '-', or Nothing
    expDigits :: String
  }
  deriving (Eq, Show)

floatLiteralToString :: TokenType -> String
floatLiteralToString (FloatLiteral (IntegerPart intPart) mFrac mExp) =
  intPart ++ fracStr ++ expStr
  where
    fracStr = case mFrac of
      Nothing -> ""
      Just (FractionalPart frac) -> "." ++ frac

    expStr = case mExp of
      Nothing -> ""
      Just (ExponentPart expSign expDigits) -> case expSign of
        Nothing -> "e" ++ "+" ++ expDigits
        Just sign -> "e" ++ [sign] ++ expDigits
floatLiteralToString otherwise = ""

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Eq, Show)

-- Lexer State & Error Types
data LexError = LexError
  { errorMsg :: String,
    errorPosition :: Position
  }
  deriving (Show)

data LexerState = LexerState
  { getInput :: String,
    currentPosition :: Position
  }
  deriving (Show)

newLexError :: String -> Position -> LexError
newLexError m p = LexError {errorMsg = m, errorPosition = p}

-- Lexer Monad Definition

newtype Lexer a = Lexer {runLexer :: LexerState -> Either LexError (a, LexerState)}

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
    Left _ -> runLexer la2 state
    Right lexed -> Right lexed

-- Basic Combinators

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

charL :: Char -> Lexer Char
charL s = satisfy (== s)

oneOrMore :: Lexer a -> Lexer [a]
oneOrMore la = (:) <$> la <*> zeroOrMore la

zeroOrMore :: Lexer a -> Lexer [a]
zeroOrMore la = oneOrMore la <|> pure []

optional :: Lexer a -> Lexer (Maybe a)
optional la = Just <$> la <|> pure Nothing

choice :: [Lexer a] -> Lexer a
choice = foldr (\b a -> a <|> (b)) emptyLexer

-- whitespace and control characters
nl :: Lexer Char
ws :: Lexer Char
cr :: Lexer Char
tab :: Lexer Char
nl = charL '\n'

ws = charL ' '

cr = charL '\r'

tab = charL '\t'

escapedLexer :: Lexer TokenType
escapedLexer = choice (map (fmap Escaped) [nl, ws, cr, tab])

emptyLexer :: Lexer a
anyNonWhitespaceLexer :: Lexer Char
anyNonWhitespaceLexer = satisfy (`notElem` ['\n', '\r', '\t', ' '])
emptyLexer = Lexer $ \state -> Left (newLexError "empty Parser failed to parse" (currentPosition state))

-- Position Tracking

advancePosition :: Char -> Position -> Position
advancePosition c pos = case c of
  '\n' -> Position {line = line pos + 1, column = 1} -- new line
  '\r' -> pos -- ignore carriage return
  '\t' -> pos {column = column pos + tabWidth} -- tab
  _ -> pos {column = column pos + 1} -- normal char
  where
    tabWidth = 4 -- configurable

withPosition :: Lexer TokenType -> Lexer Token
withPosition lexer = Lexer $ \state ->
  let pos = currentPosition state
   in case runLexer lexer state of
        Right (tokenType, newState) -> Right (Token tokenType pos, newState)
        Left err -> Left err

-- Number Lexers
intL :: Lexer String
intL = oneOrMore (satisfy isDigit)

lexInt :: Lexer IntegerPart
lexInt = IntegerPart <$> oneOrMore (satisfy isDigit)

lexFractionalPart :: Lexer FractionalPart
lexFractionalPart = FractionalPart <$> (charL '.' *> intL)

lexExponentPart :: Lexer ExponentPart
lexExponentPart =
  (charL 'e' <|> charL 'E')
    *> ( ExponentPart
           <$> optional (satisfy (`elem` ['+', '-']))
           <*> intL
       )

numberL :: Lexer Number
numberL = do
  intPart <- lexInt
  maybeFrac <- optional lexFractionalPart
  maybeExp <- optional lexExponentPart

  pure $ case (maybeFrac, maybeExp) of
    (Nothing, Nothing) -> IntNum intPart
    _ -> FloatNum intPart maybeFrac maybeExp

numberLexer :: Lexer TokenType
numberLexer = Lexer $ \state ->
  case runLexer numberL state of
    Right (x, xs) ->
      case x of
        IntNum info -> Right (IntLiteral info, xs)
        FloatNum intInfo maybeFrac maybeExponent -> Right (FloatLiteral intInfo maybeFrac maybeExponent, xs)
    Left err -> Left err

safeNumberLexer :: Lexer TokenType
safeNumberLexer = Lexer $ \state -> do
  (parsedNumber, newState) <- runLexer numberLexer state

  case getInput newState of
    (x : _)
      | isAlpha x || x == '_' || x == '$' || x == '.' ->
          Left (newLexError "illegal numeric literal" (currentPosition newState))
      | otherwise ->
          Right (parsedNumber, newState)
    [] -> Right (parsedNumber, newState)

-- String Lexers
stringL :: Lexer String
stringL = charL '"' *> zeroOrMore (satisfy (`notElem` ['"'])) <* charL '"'

stringLexer :: Lexer TokenType
stringLexer = StringLiteral <$> stringL

-- Identifier & Keyword Lexers

ident :: Lexer String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy (\c -> isAlphaNum c || c == '_' || c == '$'))

keywords :: [(String, String -> TokenType)]
keywords =
  [ ("fn", Function),
    ("let", Let),
    ("true", TrueLit),
    ("false", FalseLit),
    ("if", If),
    ("else", Else),
    ("return", Return)
  ]

lookupIdent :: String -> TokenType
lookupIdent identifier = case lookup identifier keywords of
  Just constructor -> constructor identifier
  Nothing -> Identifier identifier

identifiderAndKeywordsLexer :: Lexer TokenType
identifiderAndKeywordsLexer = lookupIdent <$> ident

-- Symbol Lexers

specialSymbols :: [(Char, Char -> TokenType)]
specialSymbols = [('(', LParen), (')', RParen), ('[', LBracket), (']', RBracket), ('}', RBrace), ('{', LBrace), (';', Semicolon), (':', Colon), (',', Comma), ('-', Minus), ('+', Plus), ('*', Asterisk), ('<', LessThan), ('>', GreaterThan), ('/', Slash)]

symbolMap :: [Char]
symbolMap = map fst specialSymbols

symbolToLexer :: (Char, Char -> TokenType) -> Lexer TokenType
symbolToLexer (s, tokenType) = tokenType <$> charL s

specialSymbolsLexer :: Lexer TokenType
specialSymbolsLexer = choice (map symbolToLexer specialSymbols)

twoCharSymbols :: [((Char, Char -> TokenType), (String, String -> TokenType))]
twoCharSymbols = [(('!', Bang), ("!=", NotEqual)), (('=', Assign), ("==", Equal))]

twoCharSymbolToLexer :: (Char, Char -> TokenType) -> (String, String -> TokenType) -> Lexer TokenType
twoCharSymbolToLexer (c, singleToken) (s, doubleToken) = doubleToken <$> traverse charL s <|> singleToken <$> charL c

specialDoubleCharSymbolsLexer :: Lexer TokenType
specialDoubleCharSymbolsLexer = choice (map (uncurry twoCharSymbolToLexer) twoCharSymbols)

-- Main Token Lexer

tokenizer :: Lexer Token
tokenizer = withPosition $ Lexer $ \state ->
  case getInput state of
    [] -> Left (newLexError "Unexpected end of input" (currentPosition state))
    c : cs
      | c == '"' -> runLexer stringLexer state
      | c == '!' -> runLexer specialDoubleCharSymbolsLexer state -- handles !=, !
      | c == '=' -> runLexer specialDoubleCharSymbolsLexer state -- handles ==, =
      | c `elem` symbolMap -> runLexer specialSymbolsLexer state
      | isDigit c -> runLexer safeNumberLexer state
      | isAlpha c || c == '_' || c == '$' -> runLexer identifiderAndKeywordsLexer state
      | c `elem` ['\n', '\t', '\n', '\r', ' '] -> runLexer (Escaped <$> (nl <|> ws <|> cr <|> tab)) state
      | otherwise -> Left (newLexError ("Unexpected character: " ++ show c) (currentPosition state))

-- Entry Points
--
nextToken :: LexerState -> Either LexError (Token, LexerState)
nextToken = runLexer tokenizer

tokenizerAll :: LexerState -> Either LexError [Token]
tokenizerAll state =
  if null (getInput state)
    then
      return []
    else do
      result <- runLexer tokenizer state
      case result of
        (token, newState) -> do
          restTokens <- tokenizerAll newState
          return (token : restTokens)

-- Helper
toTokenType :: Either LexError [Token] -> Either LexError [TokenType]
toTokenType = fmap (map (tokenType))
