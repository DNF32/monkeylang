{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Token where

import Control.Applicative (Alternative (..))
import Control.Lens
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Traversable ()
import SimpleParser

class HasToken a where
  getToken :: a -> Token

getPos :: (HasToken a) => a -> Position
getPos = _tokenPosition . getToken

instance HasToken Token where
  getToken = id

-- Core types
data TokenType
  = Illegal
  | EOF
  | Identifier String -- Keep: need the actual identifier name
  | IntLiteral String -- Keep: need the actual number
  | FloatLiteral String -- Keep: need the actual number
  | StringLiteral String -- Keep: need the actual string content
  | Assign -- Remove Char: always '='
  | Plus -- Remove Char: always '+'
  | Minus -- Remove Char: always '-'
  | Bang -- Remove Char: always '!'
  | Asterisk -- Remove Char: always '*'
  | Slash -- Remove Char: always '/'
  | Equal -- Remove String: always "=="
  | NotEqual -- Remove String: always "!="
  | LessThan -- Remove Char: always '<'
  | GreaterThan -- Remove Char: always '>'
  | Dot
  | Comma -- Remove Char: always ','
  | Semicolon -- Remove Char: always ';'
  | Colon -- Remove Char: always ':'
  | LParen -- Remove Char: always '('
  | RParen -- Remove Char: always ')'
  | LBrace -- Remove Char: always '{'
  | RBrace -- Remove Char: always '}'
  | LBracket -- Remove Char: always '['
  | RBracket -- Remove Char: always ']'
  | Function -- Remove String: always "fn"
  | Let -- Remove String: always "let"
  | TrueLit -- Remove String: always "true"
  | FalseLit -- Remove String: always "false"
  | If -- Remove String: always "if"
  | While -- Remove String: always "if"
  | Continue -- Remove String: always "if"
  | Break -- Remove String: always "if"
  | Else -- Remove String: always "else"
  | Return -- Remove String: always "return"
  | Union
  | -- type system
    IntType
  | StringType
  | BoolType
  | FloatType
  | VoidType
  | StructType
  | Mut
  | Pipe
  | And
  | Or
  | Null
  | Escaped Char -- Keep?: depends on what this is for
  deriving (Eq, Show)

data Token = Token
  { _tokenType :: TokenType,
    _tokenPosition :: Position
  }
  deriving (Eq, Show)

data Number
  = IntNum String
  | FloatNum String
  deriving (Eq, Show)

data Position = Position
  { _line :: Int,
    _column :: Int
  }
  deriving (Eq, Show)

-- Lexer State & Error Types
data LexError = LexError
  { _errorMsg :: String,
    _errorPosition :: Position
  }
  deriving (Show)

makeLenses ''Token
makeLenses ''LexError
makeLenses ''Position

newLexError :: String -> Position -> LexError
newLexError m p = LexError {_errorMsg = m, _errorPosition = p}

instance SimpleParserError LexError LexerState where
  emptyError state =
    LexError
      { _errorMsg = "Empty parser",
        _errorPosition = currentPosition state
      }

data LexerState = LexerState
  { getInput :: String,
    currentPosition :: Position
  }
  deriving (Show)

type Lexer a = SimpleParser LexerState LexError a

runLexer :: Lexer a -> LexerState -> Either LexError (a, LexerState)
runLexer = run

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

-- Basic Combinators

satisfy :: (Char -> Bool) -> Lexer Char
satisfy cb = SimpleParser $ \state ->
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

anyNonWhitespaceLexer :: Lexer Char
anyNonWhitespaceLexer = satisfy (`notElem` ['\n', '\r', '\t', ' '])

eofLexer :: Lexer TokenType
eofLexer = SimpleParser $ \state ->
  case getInput state of
    [] -> Right (EOF, state)
    _ -> Left $ newLexError "Expected end of input" (currentPosition state)

-- Position Tracking

advancePosition :: Char -> Position -> Position
advancePosition c pos = case c of
  '\n' ->
    pos
      & line %~ (+ 1)
      & column .~ 1
  '\r' -> pos
  '\t' -> pos & column %~ (+ tabWidth)
  _ -> pos & column %~ (+ 1)
  where
    tabWidth = 4 -- configurable

withPosition :: Lexer TokenType -> Lexer Token
withPosition lexer = SimpleParser $ \state ->
  let pos = currentPosition state
   in case runLexer lexer state of
        Right (tokenT, newState) -> Right (Token tokenT pos, newState)
        Left err -> Left err

-- Number Lexers
intL :: Lexer String
intL = oneOrMore (satisfy isDigit)

lexInt :: Lexer String
lexInt = oneOrMore (satisfy isDigit)

lexFractionalPart :: Lexer String
lexFractionalPart = ((:)) <$> charL '.' <*> intL

lexExponentPart :: Lexer String
lexExponentPart =
  ( \eChar x y -> case x of
      Just sign -> eChar : sign : y
      _ -> eChar : y
  )
    <$> (charL 'e' <|> charL 'E')
    <*> optional (satisfy (`elem` ['+', '-']))
    <*> intL

numberL :: Lexer TokenType
numberL = do
  intPart <- lexInt
  maybeFrac <- optional lexFractionalPart
  maybeExp <- optional lexExponentPart

  pure $ case (maybeFrac, maybeExp) of
    (Nothing, Nothing) -> IntLiteral intPart
    (Just frac, Just expPart) -> FloatLiteral (intPart ++ frac ++ expPart)
    (Just frac, _) -> FloatLiteral (intPart ++ frac)
    (_, Just expPart) -> FloatLiteral (intPart ++ expPart)

safeNumberLexer :: Lexer TokenType
safeNumberLexer = SimpleParser $ \state -> do
  (parsedNumber, newState) <- runLexer numberL state

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

keywords :: [(String, TokenType)]
keywords =
  [ ("fn", Function),
    ("let", Let),
    ("true", TrueLit),
    ("false", FalseLit),
    ("if", If),
    ("while", While),
    ("break", Break),
    ("continue", Continue),
    ("else", Else),
    ("return", Return),
    ("Null", Null),
    ("Int", IntType), -- needs its own TokenType
    ("String", StringType),
    ("Bool", BoolType), -- missing!
    ("Float", FloatType),
    ("Void", VoidType), -- missing!
    ("Union", Union),
    ("Struct", StructType),
    ("mut", Mut)
  ]

lookupIdent :: String -> TokenType
lookupIdent identifier = case lookup identifier keywords of
  Just constructor -> constructor
  Nothing -> Identifier identifier

identifiderAndKeywordsLexer :: Lexer TokenType
identifiderAndKeywordsLexer = lookupIdent <$> ident

-- Symbol Lexers

specialSymbols :: [(Char, TokenType)]
specialSymbols = [('(', LParen), (')', RParen), ('[', LBracket), (']', RBracket), ('}', RBrace), ('{', LBrace), (';', Semicolon), (':', Colon), (',', Comma), ('-', Minus), ('+', Plus), ('*', Asterisk), ('<', LessThan), ('>', GreaterThan), ('/', Slash), ('|', Pipe), ('.', Dot)]

symbolMap :: [Char]
symbolMap = map fst specialSymbols

lookupSpecialSymbols :: Char -> TokenType
lookupSpecialSymbols symbol = case lookup symbol specialSymbols of
  Just constructor -> constructor
  Nothing -> Illegal

specialSymbolsLexer :: Lexer TokenType
specialSymbolsLexer = lookupSpecialSymbols <$> satisfy (`elem` symbolMap)

twoCharSymbols :: [((Char, TokenType), (String, TokenType))]
twoCharSymbols = [(('!', Bang), ("!=", NotEqual)), (('=', Assign), ("==", Equal)), (('&', Illegal), ("&&", And)), (('|', Pipe), ("||", Or))]

twoCharSymbolToLexer :: (Char, TokenType) -> (String, TokenType) -> Lexer TokenType
twoCharSymbolToLexer (c, singleToken) (s, doubleToken) = (doubleToken <$ traverse charL s) <|> (singleToken <$ charL c)

specialDoubleCharSymbolsLexer :: Lexer TokenType
specialDoubleCharSymbolsLexer = choice (map (uncurry twoCharSymbolToLexer) twoCharSymbols)

-- Main Token Lexer

tokenizer :: Lexer Token
tokenizer = do
  tok <- tokenizer'
  case tok ^. tokenType of
    Escaped _ -> tokenizer -- Skip and get next token
    _ -> return tok

tokenizer' :: Lexer Token
tokenizer' = withPosition $ SimpleParser $ \state ->
  case getInput state of
    [] -> Right (EOF, state)
    c : cs
      | c == '"' -> runLexer stringLexer state
      | c == '!' -> runLexer specialDoubleCharSymbolsLexer state
      | c == '=' -> runLexer specialDoubleCharSymbolsLexer state
      | c == '&' -> runLexer specialDoubleCharSymbolsLexer state
      | c == '|' -> runLexer specialDoubleCharSymbolsLexer state
      | c `elem` symbolMap -> runLexer specialSymbolsLexer state
      | isDigit c -> runLexer safeNumberLexer state
      | isAlpha c || c == '_' || c == '$' -> runLexer identifiderAndKeywordsLexer state
      | c `elem` ['\n', '\t', '\r', ' '] -> runLexer (Escaped <$> (nl <|> ws <|> cr <|> tab)) state
      | otherwise -> Left (newLexError ("Unexpected character: " ++ show c) (currentPosition state))

-- Entry Points
--
nextToken :: LexerState -> Either LexError (Token, LexerState)
nextToken = runLexer tokenizer

tokenizerAll :: LexerState -> Either LexError [Token]
tokenizerAll state = do
  (token, newState) <- runLexer tokenizer state
  if token ^. tokenType == EOF
    then return [token] -- Stop, return just EOF
    else do
      restTokens <- tokenizerAll newState
      return (token : restTokens)

-- Helper
toTokenType :: Either LexError [Token] -> Either LexError [TokenType]
toTokenType = fmap (map (_tokenType))
