{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), Statement (..))
import Token (LexError, Lexer, LexerState (currentPosition), Position, Token (tokenType), TokenType (..), runLexer, tokenizer)

data ParserError
  = SyntaxError ParseError
  | TokenError LexError
  deriving (Show)

newParserError :: String -> Position -> ParserError
newParserError m p = SyntaxError (ParseError {errorMsg = m, errorPosition = p})

data ParseError = ParseError
  { errorMsg :: String,
    errorPosition :: Position
  }
  deriving (Show)

newtype Parser a = P {runParser :: LexerState -> Either ParserError (a, LexerState)}

parseLetStatement :: Parser Statement
parseLetStatement = undefined

parseExpression :: Parser Expression
parseExpression = P $ \state -> case runLexer tokenizer state of
  Left lexErr ->
    Left (TokenError lexErr)
  Right (token, newState) -> case tokenType token of
    IntLiteral intString ->
      Right
        ( IntLit {token = token, intValue = read intString},
          newState
        )
    FloatLiteral floatString ->
      Right
        ( FloatLit {floatValue = read floatString, token = token},
          newState
        )
    StringLiteral value ->
      Right
        ( StringLit {stringValue = value, token = token},
          newState
        )
    otherwise ->
      Left (newParserError "Token type parser not implement" (currentPosition state))
