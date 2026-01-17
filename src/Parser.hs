{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), Statement (..), infixToPrecedence)
import Control.Applicative (Alternative (..))
import Control.Lens
import Debug.Trace
import SimpleParser
import Token (LexError, LexerState (currentPosition), Position, Token (_tokenType), TokenType (..), runLexer, tokenPosition, tokenType, tokenizer)

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

newParserWithError :: String -> Position -> AstParser a
newParserWithError m p = SimpleParser $ \state -> (Left (newParserError m p))

expectedError :: Token -> TokenType -> AstParserError
expectedError token expectedTokenType = newParserError ("Expected" ++ show (token ^. tokenType) ++ "got" ++ show expectedTokenType) (token ^. tokenPosition)

type AstParser a = SimpleParser LexerState AstParserError a

runAstParser :: AstParser a -> LexerState -> Either AstParserError (a, LexerState)
runAstParser = run

isToken :: TokenType -> AstParser Token
isToken tp = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Right (token, newState)
      | token ^. tokenType == tp -> Right (token, newState)
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
                ("unexpected token: " ++ show (token ^. tokenType))
                (currentPosition newState)
            )

peekToken :: AstParser (Maybe Token)
peekToken = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Left _ -> Right (Nothing, state)
    Right (t, _) -> Right (Just t, state)

peekTokenType :: AstParser (Maybe TokenType)
peekTokenType = fmap (fmap _tokenType) peekToken

parseIndentifierExpression :: AstParser Expression
parseIndentifierExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          Identifier _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tok ^. tokenType of
      Identifier name ->
        IdentifierLit
          { name = name,
            token = tok
          }

parseIntExpression :: AstParser Expression
parseIntExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          IntLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tok ^. tokenType of
      IntLiteral value ->
        IntLit
          { token = tok,
            intValue = read value
          }

parseFloatExpression :: AstParser Expression
parseFloatExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          FloatLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tok ^. tokenType of
      FloatLiteral value ->
        FloatLit
          { token = tok,
            floatValue = read value
          }

parseStringExpression :: AstParser Expression
parseStringExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          StringLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tok ^. tokenType of
      StringLiteral value ->
        StringLit
          { stringValue = value,
            token = tok
          }

parseLiteralExpression :: AstParser Expression
parseLiteralExpression = choice [parseFloatExpression, parseStringExpression, parseBoolean, parseIndentifierExpression, parseIntExpression]

parseBangExpression :: AstParser Expression
parseBangExpression = PrefixExpression <$> isToken Bang <*> pure Bang <*> parseExpressionRbp 50

parseMinusExpression :: AstParser Expression
parseMinusExpression = PrefixExpression <$> isToken Minus <*> pure Minus <*> parseExpressionRbp 50

parseBoolean :: AstParser Expression
parseBoolean = parseTrueKeyword <|> parseFalseKeyword

parseTrueKeyword :: AstParser Expression
parseTrueKeyword =
  BooleanLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          TrueLit -> True
          _ -> False
      )
    <*> pure True

parseFalseKeyword :: AstParser Expression
parseFalseKeyword =
  BooleanLit
    <$> satisfyT
      ( \tok -> case tok ^. tokenType of
          FalseLit -> True
          _ -> False
      )
    <*> pure False

parseNud :: AstParser Expression
parseNud = do
  peekR <- peekToken
  case peekR of
    Just peekT | peekT ^. tokenType /= Semicolon -> do
      case peekT ^. tokenType of
        Identifier _ -> parseLiteralExpression
        IntLiteral _ -> parseLiteralExpression
        StringLiteral _ -> parseLiteralExpression
        FloatLiteral _ -> parseLiteralExpression
        TrueLit -> parseBoolean
        FalseLit -> parseBoolean
        Bang -> parseBangExpression
        Minus -> parseBangExpression
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show (peekT ^. tokenType)) (peekT ^. tokenPosition)

parseInfixExpression :: TokenType -> Expression -> AstParser Expression
parseInfixExpression tokenType expr = do
  parsedToken <- isToken tokenType
  rightExpr <- parseExpressionRbp (infixToPrecedence tokenType)
  return InfixExpression {token = parsedToken, left = expr, operator = tokenType, right = rightExpr}

parseSumInfixExpression :: Expression -> AstParser Expression
parseSumInfixExpression = parseInfixExpression Plus

parseMinusInfixExpression :: Expression -> AstParser Expression
parseMinusInfixExpression = parseInfixExpression Minus

parseMulInfixExpression :: Expression -> AstParser Expression
parseMulInfixExpression = parseInfixExpression Asterisk

parseDivInfixExpression :: Expression -> AstParser Expression
parseDivInfixExpression = parseInfixExpression Slash

traceParse :: String -> a -> a
traceParse msg x = trace ("  " ++ msg) x

parseExpressionRbp :: Integer -> AstParser Expression
parseExpressionRbp precedence = do
  expr <- parseNud
  continueInfix precedence expr

continueInfix :: Integer -> Expression -> AstParser Expression
continueInfix precedence leftExpr = do
  peekR <- peekToken
  case peekR of
    Just peekT | peekT ^. tokenType /= Semicolon && infixToPrecedence (peekT ^. tokenType) > precedence -> do
      newExpr <- case peekT ^. tokenType of
        Plus -> parseSumInfixExpression leftExpr
        Minus -> parseMinusInfixExpression leftExpr
        Asterisk -> parseMulInfixExpression leftExpr
        Slash -> parseDivInfixExpression leftExpr
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show (peekT ^. tokenType)) (peekT ^. tokenPosition)
      continueInfix precedence newExpr
    _ -> return leftExpr

parseLetStatement :: AstParser Statement
parseLetStatement =
  LetStatement
    <$> isToken Let
    <*> parseIndentifierExpression
    <* isToken Assign
    <*> parseExpressionRbp 0
    <* optional (isToken Semicolon)

parseReturnStatement :: AstParser Statement
parseReturnStatement = ReturnStatement <$> isToken Return <*> parseExpressionRbp 0 <* optional (isToken Semicolon)

parseExpressionStatement :: Token -> AstParser Statement
parseExpressionStatement token = ExpressionStatement <$> pure token <*> parseExpressionRbp 0 <* optional (isToken Semicolon)

parseStatement :: AstParser Statement
parseStatement = do
  maybeToken <- peekToken
  case maybeToken of
    Just tok -> do
      case tok ^. tokenType of
        Let -> parseLetStatement
        Return -> parseReturnStatement
        _ -> parseExpressionStatement tok

parseProgram :: AstParser [Statement]
parseProgram = oneOrMore parseStatement
