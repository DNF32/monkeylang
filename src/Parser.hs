{-# LANGUAGE DuplicateRecordFields #-}

module Parser where

import Ast (Expression (..), FieldDecl (..), FieldInitialization (..), Param (..), Precedence (..), Statement (..), infixToPrecedence)
import Control.Applicative (Alternative (..))
import Control.Lens
import Data.Char (GeneralCategory (LowercaseLetter))
import Data.List (intercalate)
import Data.Map qualified as Map
import Debug.Trace
import SimpleParser
import Token
import Types

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

parseIdentifierExpression :: AstParser Expression
parseIdentifierExpression =
  tokenToLit
    <$> satisfyT isIdentifier
  where
    isIdentifier :: Token -> Bool
    isIdentifier (Token (Identifier _) _) = True
    isIdentifier _ = False
    tokenToLit tok@(Token (Identifier name) _) =
      IdentifierLit
        { name = name,
          token = tok
        }

parseParam :: AstParser Param
parseParam = expressionToParam <$> parseIdentifierExpression <*> optional parseTypeHint

expressionToParam :: Expression -> Maybe Type -> Param
expressionToParam identifier@(IdentifierLit tok name) ty = Param {paramToken = tok, paramName = name, paramType = ty}
expressionToParam expr _ =
  error $ "expressionToParam called on non-IdentifierLit: " ++ show expr

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
parseLiteralExpression = choice [parseFloatExpression, parseStringExpression, parseBoolean, parseIdentifierExpression, parseIntExpression]

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
parseCallExpression expr = case expr of
  IdentifierLit tok _ -> CallExpression tok expr <$> parseArgs
  FunctionLit tok _ _ _ -> CallExpression tok expr <$> parseArgs
  CallExpression tok _ _ -> CallExpression tok expr <$> parseArgs
  FieldAccess tok _ _ -> CallExpression tok expr <$> parseArgs
  _ ->
    let tok = (token :: Expression -> Token) expr
     in newParserWithError
          ("Tried to create a Call expression without IdentifierLit or FunctionLit, found: " ++ show (expr))
          (_tokenPosition tok)

parseParameters :: AstParser [Param]
parseParameters = isToken LParen *> sepBy parseParam (isToken Comma) <* isToken RParen

--
-- parseParameters :: AstParser [Expression]
-- parseParameters = choice [parseZeroParameters, parseOneOrMoreParameter]

-- parseOneOrMoreParameter :: AstParser [Expression]
-- parseOneOrMoreParameter = isToken LParen *> parseOneOrMoreParameter' <* isToken RParen
--  where
--    parseOneOrMoreParameter' = (:) <$> parseIdentifierExpression <*> zeroOrMore (isToken Comma *> parseIndentifierExpression)
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
        Dot -> parseFieldAccessExpression leftExpr
        LBrace -> parseStructInit leftExpr
        _ -> newParserWithError ("Unexpected token in infix position: " ++ show tt) pos
      continueInfix precedence newExpr
    _ -> return leftExpr

parseFieldAccessExpression :: Expression -> AstParser Expression
parseFieldAccessExpression leftExpr = do
  dotTok <- isToken Dot
  identLit <- parseIdentifierExpression
  let fieldName = case identLit of
        IdentifierLit _ name -> name
        _ -> error "parseIdentifierExpression returned non-identifier" -- should never happen
  return (FieldAccess dotTok leftExpr fieldName)

parseLetStatement :: AstParser Statement
parseLetStatement = do
  tok <- isToken Let
  identifier <- parseIdentifierExpression
  hint <- optional parseTypeHint
  _ <- isToken Assign
  expr <- parseExpressionRbp LOWEST
  _ <- optional (isToken Semicolon)
  return (LetStatement tok identifier expr hint)

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
  expr <- parseIdentifierExpression
  case expr of
    IdentifierLit _ name -> return (UnresolvedT name)
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
--    ident <- parseIdentifierExpression

parseReturnStatement :: AstParser Statement
parseReturnStatement = ReturnStatement <$> isToken Return <*> parseExpressionRbp LOWEST <* optional (isToken Semicolon)

parseStructDecl :: AstParser Statement
parseStructDecl = do
  tok <- isToken StructType
  nameExpr <- parseIdentifierExpression
  _ <- isToken LBrace
  fields <- oneOrMore parseFieldDeclaration
  _ <- isToken RBrace
  _ <- optional (isToken Semicolon)
  case nameExpr of
    IdentifierLit _ n -> return (StructDecl tok n fields)
    _ -> newParserWithError "Expected struct name" (_tokenPosition tok)
  where
    parseFieldDeclaration = do
      pos <- getCurrentPosition
      nameExpr <- parseIdentifierExpression
      typeHint <- parseTypeHint
      _ <- optional (isToken Semicolon)
      case nameExpr of
        IdentifierLit fieldTok n -> return (FieldDecl fieldTok n typeHint)
        _ -> newParserWithError "Expected field name" pos

parseStructInit :: Expression -> AstParser Expression
parseStructInit leftExpr = do
  tok <- isToken LBrace
  inits <- oneOrMore parseFieldInit
  _ <- isToken RBrace
  case leftExpr of
    IdentifierLit _ name -> return (StructInitialization tok name inits)
    _ -> newParserWithError "Expected struct name before {" (getPos leftExpr)
  where
    parseFieldInit :: AstParser FieldInitialization
    parseFieldInit = do
      varName <- parseIdentifierExpression
      _ <- isToken Colon
      expr <- parseExpressionRbp LOWEST
      _ <- optional (isToken Semicolon)
      case varName of
        IdentifierLit tok name -> return (FieldInit tok name expr)
        _ -> newParserWithError "Expected field name" (getPos varName)

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
        StructType -> fmap Just parseStructDecl
        _ | tt == endToken -> pure Nothing
        _ -> fmap Just (parseExpressionStatement tok)

parseStatementInScope :: TokenType -> AstParser (Maybe Statement)
parseStatementInScope endToken = do
  pTok <- peekToken
  case pTok of
    tok@(Token tt _) -> do
      case tt of
        Let -> fmap Just parseLetStatement
        Return -> fmap Just parseReturnStatement
        _ | tt == endToken -> pure Nothing
        _ -> fmap Just (parseExpressionStatement tok)

parseProgram :: AstParser Statement
parseProgram = Program <$> parseStatementsUntil parseStatement EOF

parseStatementsUntil :: (TokenType -> AstParser (Maybe Statement)) -> TokenType -> AstParser [Statement]
parseStatementsUntil parsingFunction endToken = do
  maybeStmt <- parsingFunction endToken
  case maybeStmt of
    Nothing -> pure []
    Just stmt -> (stmt :) <$> parseStatementsUntil parsingFunction endToken

parseBlockStatement :: AstParser Statement
parseBlockStatement = BlockStatement <$> parseStatementsUntil parseStatementInScope RBrace

parseIfExpression :: AstParser Expression
parseIfExpression = do
  ifTok <- isToken If
  condition <- parseGroupExpression

  _ <- isToken LBrace
  consequence <- parseStatementsUntil parseStatementInScope RBrace
  _ <- isToken RBrace

  alternative <- do
    pTok <- peekToken
    case pTok of
      Token Else _ -> do
        _ <- isToken Else
        _ <- isToken LBrace
        alt <- parseStatementsUntil parseStatementInScope RBrace
        _ <- isToken RBrace
        return (Just alt)
      _ ->
        return Nothing

  return (IfExpression ifTok condition consequence alternative)
