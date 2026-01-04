{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), Statement (..))
import SimpleParser
import Token (LexError, LexerState (currentPosition), Position, Token (Token, tokenPosition, tokenType), TokenType (..), runLexer, tokenizer)

data AstParserError
  = SyntaxError ParserError
  | TokenError LexError
  deriving (Show)

data ParserError = ParserError
  { errorMsg :: String,
    errorPosition :: Position
  }
  deriving (Show)

instance SimpleParserError ParserError LexerState where
  emptyError state =
    ParserError
      { errorMsg = "Empty parser",
        errorPosition = currentPosition state
      }

instance SimpleParserError AstParserError LexerState where
  emptyError state = SyntaxError (emptyError state)

newParserError :: String -> Position -> AstParserError
newParserError m p = SyntaxError (ParserError {errorMsg = m, errorPosition = p})

expectedError :: Token -> TokenType -> AstParserError
expectedError token expectedTokenType = newParserError ("Expected" ++ show (tokenType token) ++ "got" ++ show expectedTokenType) (tokenPosition token)

type AstParser a = SimpleParser LexerState AstParserError a

runAstParser :: AstParser a -> LexerState -> Either AstParserError (a, LexerState)
runAstParser = run

parseLet :: AstParser TokenType
parseLet = undefined

isToken :: TokenType -> AstParser Token
isToken tp = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Right (token, newState)
      | tokenType token == tp -> Right (token, newState)
      | otherwise -> Left (expectedError token tp)
    Left err -> Left (TokenError err)

satisfyT :: (Token -> Bool) -> AstParser Token
satisfyT predicate = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Left lexErr ->
      Left (TokenError lexErr)
    Right (token, newState)
      | predicate token ->
          Right (token, newState)
      | otherwise ->
          Left
            ( newParserError
                ("unexpected token: " ++ show (tokenType token))
                (currentPosition newState)
            )

parseIdentifiderNode :: AstParser Expression
parseIdentifiderNode =
  tokenToIdentifierNode
    <$> satisfyT
      ( \tok -> case tokenType tok of
          Identifier _ -> True
          _ -> False
      )
  where
    tokenToIdentifierNode tok@(Token {tokenType = Identifier name}) =
      IdentifierLit
        { name = name,
          token = tok
        }

parseLetStatement :: AstParser Statement
parseLetStatement =
  LetStatement
    <$> isToken Let
    <*> parseIdentifiderNode
    <* isToken Assign
    <*> parseExpression

parseReturnStatement :: AstParser Statement
parseReturnStatement = ReturnStatement <$> isToken Return <*> parseExpression

parseExpression :: AstParser Expression
parseExpression = SimpleParser $ \state -> case runLexer tokenizer state of
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
