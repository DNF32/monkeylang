{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), Statement (..), infixToPrecedence)
import Control.Applicative (Alternative (..))
import Debug.Trace
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

newParserWithError :: String -> Position -> AstParser a
newParserWithError m p = SimpleParser $ \state -> (Left (newParserError m p))

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

peekToken :: AstParser (Maybe Token)
peekToken = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Left _ -> Right (Nothing, state)
    Right (t, _) -> Right (Just t, state)

peekTokenType :: AstParser (Maybe TokenType)
peekTokenType = fmap (fmap tokenType) peekToken

parseLetStatement :: AstParser Statement
parseLetStatement =
  LetStatement
    <$> isToken Let
    <*> parseIndentifierExpression
    <* isToken Assign
    <*> parseExpressionRbp 0

parseReturnStatement :: AstParser Statement
parseReturnStatement = ReturnStatement <$> isToken Return <*> parseExpressionRbp 0

parseIndentifierExpression :: AstParser Expression
parseIndentifierExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tokenType tok of
          Identifier _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tokenType tok of
      Identifier name ->
        IdentifierLit
          { name = name,
            token = tok
          }

parseIntExpression :: AstParser Expression
parseIntExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tokenType tok of
          IntLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tokenType tok of
      IntLiteral value ->
        IntLit
          { token = tok,
            intValue = read value
          }

parseFloatExpression :: AstParser Expression
parseFloatExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tokenType tok of
          FloatLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tokenType tok of
      FloatLiteral value ->
        FloatLit
          { token = tok,
            floatValue = read value
          }

parseStringExpression :: AstParser Expression
parseStringExpression =
  tokenToLit
    <$> satisfyT
      ( \tok -> case tokenType tok of
          StringLiteral _ -> True
          _ -> False
      )
  where
    tokenToLit tok = case tokenType tok of
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
      ( \tok -> case tokenType tok of
          TrueLit -> True
          _ -> False
      )
    <*> pure True

parseFalseKeyword :: AstParser Expression
parseFalseKeyword =
  BooleanLit
    <$> satisfyT
      ( \tok -> case tokenType tok of
          FalseLit -> True
          _ -> False
      )
    <*> pure False

parseNud :: AstParser Expression
-- parseNud = choice [parseBoolean, parseMinusExpression, parseBangExpression, parseLiteralExpression]
parseNud = do
  peekR <- peekToken
  case peekR of
    Just peekT | tokenType peekT /= Semicolon && infixToPrecedence (tokenType peekT) > precedence -> do
      newExpr <- case tokenType peekT of
        Plus -> parseSumInfixExpression leftExpr
        Minus -> parseMinusInfixExpression leftExpr
        Asterisk -> parseMulInfixExpression leftExpr
        Slash -> parseDivInfixExpression leftExpr
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show (tokenType peekT)) (tokenPosition peekT)

parseInfixExpression :: TokenType -> Expression -> AstParser Expression
parseInfixExpression tokenType expr = trace ("started an parseInfixExpression for " ++ show tokenType) $ do
  parsedToken <- isToken tokenType
  traceM ("IsToken returned" ++ show parsedToken)
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
  traceM ("parseExpressionRbp got expr = " ++ show expr)
  continueInfix precedence expr

continueInfix :: Integer -> Expression -> AstParser Expression
continueInfix precedence leftExpr = do
  peekR <- peekToken
  case peekR of
    Just peekT | tokenType peekT /= Semicolon && infixToPrecedence (tokenType peekT) > precedence -> do
      newExpr <- case tokenType peekT of
        Plus -> parseSumInfixExpression leftExpr
        Minus -> parseMinusInfixExpression leftExpr
        Asterisk -> parseMulInfixExpression leftExpr
        Slash -> parseDivInfixExpression leftExpr
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show (tokenType peekT)) (tokenPosition peekT)
      continueInfix precedence newExpr
    _ -> return leftExpr
