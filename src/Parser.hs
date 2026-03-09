{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), Precedence (..), Statement (..), exprToken, infixToPrecedence)
import Control.Applicative (Alternative (..))
import Control.Lens
import Data.Char (GeneralCategory (LowercaseLetter))
import Data.List (intercalate)
import Data.Map qualified as Map
import Debug.Trace
import SimpleParser
import Token
import TypeChecker

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

peekToken :: AstParser Token
peekToken = SimpleParser $ \state ->
  case runLexer tokenizer state of
    Right (tok, _) -> Right (tok, state) -- Don't consume, just peek
    Left err -> Left (TokenError err)

getCurrentPosition :: AstParser Position
getCurrentPosition = SimpleParser $ \state -> Right (currentPosition state, state)

parseIndentifierExpression :: AstParser Expression
parseIndentifierExpression =
  tokenToLit
    <$> satisfyT isIdentifier
  where
    isIdentifier :: Token -> Bool
    isIdentifier (Token (Identifier _) _) = True
    isIdentifier _ = False
    tokenToLit tok@(Token (Identifier name) _) =
      IdentifierLit
        { name = name,
          token = tok,
          ann = Nothing
        }

parseIdentifierExpressionWithTypeHint :: AstParser Expression
parseIdentifierExpressionWithTypeHint = identifierWithTypeHint <$> parseIndentifierExpression <*> optional parseTypeHint

identifierWithTypeHint :: Expression -> Maybe Type -> Expression
identifierWithTypeHint identifier@(IdentifierLit {}) ty =
  identifier {ann = ty}
identifierWithTypeHint expr _ =
  error $ "identifierWithTypeHint called on non-IdentifierLit: " ++ show expr

parseIntExpression :: AstParser Expression
parseIntExpression =
  tokenToLit <$> satisfyT isIntLiteral
  where
    isIntLiteral :: Token -> Bool
    isIntLiteral (Token (IntLiteral _) _) = True
    isIntLiteral _ = False

    tokenToLit :: Token -> Expression
    tokenToLit tok@(Token (IntLiteral value) _) =
      IntLit
        { token = tok,
          intValue = read value
        }

parseFloatExpression :: AstParser Expression
parseFloatExpression =
  tokenToLit <$> satisfyT isFloatLiteral
  where
    isFloatLiteral :: Token -> Bool
    isFloatLiteral (Token (FloatLiteral _) _) = True
    isFloatLiteral _ = False

    tokenToLit :: Token -> Expression
    tokenToLit tok@(Token (FloatLiteral value) _) =
      FloatLit
        { token = tok,
          floatValue = read value
        }

parseStringExpression :: AstParser Expression
parseStringExpression =
  tokenToLit <$> satisfyT isStringLiteral
  where
    isStringLiteral :: Token -> Bool
    isStringLiteral (Token (StringLiteral _) _) = True
    isStringLiteral _ = False

    tokenToLit :: Token -> Expression
    tokenToLit tok@(Token (StringLiteral value) _) =
      StringLit
        { stringValue = value,
          token = tok
        }

parseLiteralExpression :: AstParser Expression
parseLiteralExpression = choice [parseFloatExpression, parseStringExpression, parseBoolean, parseIndentifierExpression, parseIntExpression]

parseBangExpression :: AstParser Expression
parseBangExpression = PrefixExpression <$> isToken Bang <*> pure Bang <*> parseExpressionRbp PREFIX

parseMinusExpression :: AstParser Expression
parseMinusExpression = PrefixExpression <$> isToken Minus <*> pure Minus <*> parseExpressionRbp PREFIX

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

parseNull :: AstParser Expression
parseNull = NullLit <$> isToken Null

parseGroupExpression :: AstParser Expression
parseGroupExpression = isToken LParen *> parseExpressionRbp LOWEST <* isToken RParen

parseCallExpression :: Expression -> AstParser Expression
parseCallExpression expr@(IdentifierLit tok _ _) = CallExpression tok expr <$> parseArgs
parseCallExpression expr@(FunctionLit tok _ _ _) = CallExpression tok expr <$> parseArgs
parseCallExpression expr@(CallExpression tok _ _) = CallExpression tok expr <$> parseArgs
parseCallExpression expr =
  newParserWithError
    ("Tried to create a Call expression without IdentifierLit or Function Lit, found :" ++ show (expr ^. exprToken . tokenType))
    (expr ^. exprToken . tokenPosition)

parseParameters :: AstParser [Expression]
parseParameters = isToken LParen *> sepBy parseIdentifierExpressionWithTypeHint (isToken Comma) <* isToken RParen

--
-- parseParameters :: AstParser [Expression]
-- parseParameters = choice [parseZeroParameters, parseOneOrMoreParameter]

-- parseOneOrMoreParameter :: AstParser [Expression]
-- parseOneOrMoreParameter = isToken LParen *> parseOneOrMoreParameter' <* isToken RParen
--  where
--    parseOneOrMoreParameter' = (:) <$> parseIndentifierExpression <*> zeroOrMore (isToken Comma *> parseIndentifierExpression)
--
-- parseZeroParameters :: AstParser [Expression]
-- parseZeroParameters = isToken LParen *> isToken RParen *> pure []

parseArgs :: AstParser [Expression]
parseArgs = isToken LParen *> choice [parseZeroExprs RParen, parseOneOrMoreExprs RParen]

parseArrayElements :: AstParser [Expression]
parseArrayElements = choice [parseZeroExprs RBracket, parseOneOrMoreExprs RBracket]

parseDelimitedExprs :: TokenType -> AstParser [Expression]
parseDelimitedExprs end = choice [parseOneOrMoreExprs end, parseZeroExprs end]

parseOneOrMoreExprs :: TokenType -> AstParser [Expression]
parseOneOrMoreExprs end = parseOneOrMoreArgs' <* isToken end
  where
    parseOneOrMoreArgs' = (:) <$> parseExpressionRbp LOWEST <*> zeroOrMore (isToken Comma *> parseExpressionRbp LOWEST)

parseZeroExprs :: TokenType -> AstParser [Expression]
parseZeroExprs end = isToken end *> pure []

parseArrayLiteral :: AstParser Expression
parseArrayLiteral = ArrayLit <$> isToken LBracket <*> parseArrayElements

parseFunctionLiteral :: AstParser Expression
parseFunctionLiteral = do
  fn <- isToken Function
  parameters <- parseParameters
  returnType <- optional parseTypeHint
  _ <- isToken LBrace
  stmts <- parseBlockStatement
  _ <- isToken RBrace
  case stmts of
    BlockStatement stmts' ->
      return $ FunctionLit fn parameters returnType stmts'
    _ -> error "parseBlockStatement should always return BlockStatement"

parseIndexExpression :: Expression -> AstParser Expression
parseIndexExpression left = do
  tok <- isToken LBracket
  indexParsed <- parseExpressionRbp LOWEST
  _ <- isToken RBracket
  return (IndexExpression tok left indexParsed)

parseNud :: AstParser Expression
parseNud = do
  pTok <- peekToken
  case pTok of
    Token tt pos -> do
      case tt of
        Null -> parseNull
        Identifier _ -> parseLiteralExpression
        IntLiteral _ -> parseLiteralExpression
        StringLiteral _ -> parseLiteralExpression
        FloatLiteral _ -> parseLiteralExpression
        LBracket -> parseArrayLiteral
        TrueLit -> parseBoolean
        FalseLit -> parseBoolean
        Bang -> parseBangExpression
        Minus -> parseMinusExpression
        Function -> parseFunctionLiteral
        LParen -> parseGroupExpression
        If -> parseIfExpression
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show tt) pos

parseInfixExpression :: TokenType -> Expression -> AstParser Expression
parseInfixExpression tt expr = do
  parsedToken <- isToken tt
  rightExpr <- parseExpressionRbp (infixToPrecedence tt)
  return InfixExpression {token = parsedToken, left = expr, operator = tt, right = rightExpr}

parseSumInfixExpression :: Expression -> AstParser Expression
parseSumInfixExpression = parseInfixExpression Plus

parseMinusInfixExpression :: Expression -> AstParser Expression
parseMinusInfixExpression = parseInfixExpression Minus

parseMulInfixExpression :: Expression -> AstParser Expression
parseMulInfixExpression = parseInfixExpression Asterisk

parseDivInfixExpression :: Expression -> AstParser Expression
parseDivInfixExpression = parseInfixExpression Slash

parseEqualsInfixExpression :: Expression -> AstParser Expression
parseEqualsInfixExpression = parseInfixExpression Equal

parseNEqualsInfixExpression :: Expression -> AstParser Expression
parseNEqualsInfixExpression = parseInfixExpression NotEqual

parseLessThanInfixExpression :: Expression -> AstParser Expression
parseLessThanInfixExpression = parseInfixExpression LessThan

parserGreaterThanInfixExpression :: Expression -> AstParser Expression
parserGreaterThanInfixExpression = parseInfixExpression GreaterThan

traceParse :: String -> a -> a
traceParse msg x = trace ("  " ++ msg) x

parseExpressionRbp :: Precedence -> AstParser Expression
parseExpressionRbp precedence = do
  expr <- parseNud
  continueInfix precedence expr

continueInfix :: Precedence -> Expression -> AstParser Expression
continueInfix precedence leftExpr = do
  peekR <- peekToken
  case peekR of
    Token tt pos | tt /= Semicolon && infixToPrecedence tt > precedence -> do
      newExpr <- case tt of
        Plus -> parseSumInfixExpression leftExpr
        Minus -> parseMinusInfixExpression leftExpr
        Asterisk -> parseMulInfixExpression leftExpr
        Slash -> parseDivInfixExpression leftExpr
        Equal -> parseEqualsInfixExpression leftExpr
        NotEqual -> parseNEqualsInfixExpression leftExpr
        LessThan -> parseLessThanInfixExpression leftExpr
        GreaterThan -> parserGreaterThanInfixExpression leftExpr
        LBracket -> parseIndexExpression leftExpr
        LParen -> parseCallExpression leftExpr
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show tt) pos
      continueInfix precedence newExpr
    _ -> return leftExpr

parseLetStatement :: AstParser Statement
parseLetStatement = do
  tok <- isToken Let
  identifier <- parseIdentifierExpressionWithTypeHint
  _ <- isToken Assign
  expr <- parseExpressionRbp LOWEST
  _ <- optional (isToken Semicolon)
  return (LetStatement tok identifier expr)

parseTypeHint :: AstParser Type
parseTypeHint = do
  _ <- isToken Colon
  parseBaseType

parseBaseType :: AstParser Type
parseBaseType =
  choice
    [ parseNative,
      parseUnresolvedStruct
    ]

parseNative :: AstParser Type
parseNative = do
  choice (map toParser tokenToType)
  where
    tokenToType :: [(TokenType, Type)]
    tokenToType =
      [ (IntType, IntT),
        (StringType, StringT),
        (BoolType, BoolT),
        (FloatType, FloatT), -- need FloatT in your Type!
        (VoidType, VoidT),
        (AnyType, AnyT)
      ]
    toParser (tokenT, t) = t <$ isToken tokenT

parseUnresolvedStruct :: AstParser Type
parseUnresolvedStruct = do
  pos <- getCurrentPosition
  expr <- parseIndentifierExpression
  case expr of
    IdentifierLit _ name _ -> return (StructT name Map.empty)
    _ -> newParserWithError "Expected type name" pos

-- parseArrayTypeHint :: AstParser Token
-- parseArrayTypeHint = do
--  _ <- isToken Colon
--  _ <- isToken LBracket
--  _ <- isToken RBracket
--  - <- parseTypeHint

-- parseLetStatement' :: AstParser Statement
-- parseLetStatement' = do
--    <- isToken Let
--    ident <- parseIndentifierExpression

parseReturnStatement :: AstParser Statement
parseReturnStatement = ReturnStatement <$> isToken Return <*> parseExpressionRbp LOWEST <* optional (isToken Semicolon)

parseExpressionStatement :: Token -> AstParser Statement
parseExpressionStatement token = ExpressionStatement <$> pure token <*> parseExpressionRbp LOWEST <* optional (isToken Semicolon)

parseStatement :: TokenType -> AstParser (Maybe Statement)
parseStatement endToken = do
  pTok <- peekToken
  case pTok of
    tok@(Token tt _) -> do
      case tt of
        Let -> fmap Just parseLetStatement
        Return -> fmap Just parseReturnStatement
        _ | tt == endToken -> pure Nothing
        _ -> fmap Just (parseExpressionStatement tok)

parseProgram :: AstParser Statement
parseProgram = Program <$> parseStatementsUntil EOF

parseStatementsUntil :: TokenType -> AstParser [Statement]
parseStatementsUntil endToken = do
  maybeStmt <- parseStatement endToken
  case maybeStmt of
    Nothing -> pure []
    Just stmt -> (stmt :) <$> parseStatementsUntil endToken

parseBlockStatement :: AstParser Statement
parseBlockStatement = BlockStatement <$> parseStatementsUntil RBrace

parseIfExpression :: AstParser Expression
parseIfExpression = do
  ifTok <- isToken If
  condition <- parseGroupExpression

  _ <- isToken LBrace
  consequence <- parseStatementsUntil RBrace
  _ <- isToken RBrace

  alternative <- do
    pTok <- peekToken
    case pTok of
      Token Else _ -> do
        _ <- isToken Else
        _ <- isToken LBrace
        alt <- parseStatementsUntil RBrace
        _ <- isToken RBrace
        return (Just alt)
      _ ->
        return Nothing

  return (IfExpression ifTok condition consequence alternative)
