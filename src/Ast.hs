{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast
  ( Expression (..),
    Statement (..),
    FieldInitialization (..),
    TFieldInitialization (..),
    TExpression (..),
    Binding (..),
    TStatement (..),
    Param (..),
    TParam (..),
    FieldDecl (..),
    getType,
    (?==),
    Precedence (..),
    prettyPrintStatement,
    expressionToString,
    statementToString,
    infixToPrecedence,
    -- Expression lenses
    -- Statement lenses
  )
where

import Data.List (intercalate)
import Token (HasPos (..), HasToken (..), Position (..), Token (..), TokenType (..))
import Types

type Operator = TokenType

data Expression
  = IntLit {token :: Token, intValue :: Int}
  | FloatLit {token :: Token, floatValue :: Float}
  | StringLit {token :: Token, stringValue :: String}
  | IdentifierLit {token :: Token, name :: String}
  | ArrayLit {token :: Token, elements :: [Expression]}
  | BooleanLit {token :: Token, value :: Bool}
  | NullLit {token :: Token}
  | PrefixExpression {token :: Token, operator :: Operator, right :: Expression}
  | InfixExpression {token :: Token, left :: Expression, operator :: Operator, right :: Expression}
  | FunctionLit {token :: Token, parameters :: [Param], returnType :: Maybe Type, body :: [Statement]} -- Expression!
  | CallExpression {token :: Token, function :: Expression, arguments :: [Expression]}
  | IndexExpression {token :: Token, left :: Expression, index :: Expression}
  | IfExpression {token :: Token, condition :: Expression, consequence :: [Statement], alternative :: Maybe [Statement]}
  | FieldAccess {token :: Token, object :: Expression, fieldName :: String}
  | StructInitialization {token :: Token, structName :: String, fieldInits :: [FieldInitialization]}
  deriving (Eq, Show)

data TFieldInitialization = TFieldInit
  { tInitToken :: Token,
    tInitFieldName :: String,
    tInitValue :: TExpression,
    ty :: Type
  }
  deriving (Eq, Show)

exprToken :: Expression -> Token
exprToken expr =
  case expr of
    IntLit t _ -> t
    FloatLit t _ -> t
    StringLit t _ -> t
    IdentifierLit t _ -> t
    ArrayLit t _ -> t
    BooleanLit t _ -> t
    NullLit t -> t
    PrefixExpression t _ _ -> t
    InfixExpression t _ _ _ -> t
    FunctionLit t _ _ _ -> t
    CallExpression t _ _ -> t
    IndexExpression t _ _ -> t
    IfExpression t _ _ _ -> t
    FieldAccess t _ _ -> t
    StructInitialization t _ _ -> t

instance HasToken Expression where
  getToken = exprToken
  getTokenType = getTokenType . exprToken

instance HasPos Expression where
  getPos expr = getPos $ exprToken expr
  getMaybePos expr = Just $ getPos expr

data FieldInitialization = FieldInit
  { initToken :: Token,
    initFieldName :: String,
    initValue :: Expression
  }
  deriving (Eq, Show)

data Param = Param
  { paramToken :: Token,
    paramName :: String,
    paramType :: Type,
    paramMut :: Mutability
  }
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fieldToken :: Token,
    fieldName :: String,
    fieldType :: Type
  }
  deriving (Eq, Show)

infix 4 ?==

data Statement
  = Program {statements :: [Statement]}
  | LetStatement {stmtToken :: Token, name :: Expression, value :: Expression, ann :: Maybe Type, mutPer :: Mutability}
  | AssignmentStatement {stmtToken :: Token, name :: Expression, value :: Expression}
  | ReturnStatement {stmtToken :: Token, result :: Expression}
  | ExpressionStatement {stmtToken :: Token, expr :: Expression}
  | BlockStatement {statements :: [Statement]}
  | StructDecl {stmtToken :: Token, structName :: String, fields :: [FieldDecl]}
  deriving (Eq, Show)

data Precedence = LOWEST | LOGICAL_OR | LOGICAL_AND | EQUALS | LESSGREATER | SUM | PRODUCT | PREFIX | CALL | INDEX deriving (Ord, Eq, Show)

infixToPrecedence :: TokenType -> Precedence
infixToPrecedence token = case token of
  Or -> LOGICAL_OR
  And -> LOGICAL_AND
  Equal -> EQUALS
  NotEqual -> EQUALS
  LessThan -> LESSGREATER
  GreaterThan -> LESSGREATER
  Plus -> SUM
  Minus -> SUM
  Slash -> PRODUCT
  Asterisk -> PRODUCT
  LParen -> CALL
  LBracket -> INDEX
  Dot -> CALL
  LBrace -> CALL -- struct initialization: Point { x: 10 }
  otherwise -> LOWEST

statementToString :: Statement -> String
statementToString (LetStatement _ name value ann mulPer) = "let " ++ expressionToString name ++ show ann ++ " = " ++ expressionToString value
statementToString (ReturnStatement _ value) = "return " ++ expressionToString value
statementToString (ExpressionStatement _ value) = expressionToString value
statementToString (BlockStatement stmts) = intercalate "" (map statementToString stmts)
statementToString (Program stmts) = intercalate "" (map statementToString stmts)

prettyPrintStatement :: Int -> Statement -> String
prettyPrintStatement indent stmt = case stmt of
  LetStatement _ name value ann mutPer ->
    spaces indent
      ++ "LetStatement\n"
      ++ spaces (indent + 2)
      ++ "name: "
      ++ expressionToString name
      ++ "ann: "
      ++ show ann
      ++ "\n"
      ++ spaces (indent + 2)
      ++ "value:\n"
      ++ prettyPrintExpression (indent + 4) value
  ExpressionStatement _ expr ->
    spaces indent
      ++ "ExpressionStatement\n"
      ++ prettyPrintExpression (indent + 2) expr
  ReturnStatement _ expr ->
    spaces indent
      ++ "ReturnStatement\n"
      ++ prettyPrintExpression (indent + 2) expr
  BlockStatement stmts ->
    spaces indent
      ++ "BlockStatement\n"
      ++ intercalate "" (map (prettyPrintStatement (indent + 2)) stmts)
  Program stmts ->
    "Program\n"
      ++ intercalate "" (map (prettyPrintStatement 2) stmts)
  where
    spaces n = replicate n ' '

prettyPrintExpression :: Int -> Expression -> String
prettyPrintExpression indent expr = case expr of
  IntLit _ value ->
    spaces indent ++ "IntLit: " ++ show value ++ "\n"
  FloatLit _ value ->
    spaces indent ++ "FloatLit: " ++ show value ++ "\n"
  StringLit _ value ->
    spaces indent ++ "StringLit: " ++ show value ++ "\n"
  NullLit _ ->
    spaces indent ++ "NullLit\n"
  IdentifierLit _ value ->
    spaces indent ++ "IdentifierLit: " ++ value ++ "\n"
  BooleanLit _ value ->
    spaces indent ++ "BooleanLit: " ++ show value ++ "\n"
  ArrayLit _ elements ->
    spaces indent
      ++ "ArrayLit\n"
      ++ spaces (indent + 2)
      ++ "elements:\n"
      ++ intercalate "" (map (prettyPrintExpression (indent + 4)) elements)
  PrefixExpression _ op right ->
    spaces indent
      ++ "PrefixExpression ("
      ++ show op
      ++ ")\n"
      ++ spaces (indent + 2)
      ++ "right:\n"
      ++ prettyPrintExpression (indent + 4) right
  InfixExpression _ left op right ->
    spaces indent
      ++ "InfixExpression ("
      ++ show op
      ++ ")\n"
      ++ spaces (indent + 2)
      ++ "left:\n"
      ++ prettyPrintExpression (indent + 4) left
      ++ spaces (indent + 2)
      ++ "right:\n"
      ++ prettyPrintExpression (indent + 4) right
  FunctionLit _ params _ body ->
    spaces indent
      ++ "FunctionLit\n"
      ++ spaces (indent + 2)
      ++ "parameters: ["
      ++ intercalate ", " (map renderParam params)
      ++ "]\n"
      ++ spaces (indent + 2)
      ++ "body:\n"
      ++ intercalate "" (map (prettyPrintStatement (indent + 4)) body)
  CallExpression _ func args ->
    spaces indent
      ++ "CallExpression\n"
      ++ spaces (indent + 2)
      ++ "function:\n"
      ++ prettyPrintExpression (indent + 4) func
      ++ spaces (indent + 2)
      ++ "arguments:\n"
      ++ intercalate "" (map (prettyPrintExpression (indent + 4)) args)
  IndexExpression _ left idx ->
    spaces indent
      ++ "IndexExpression\n"
      ++ spaces (indent + 2)
      ++ "left:\n"
      ++ prettyPrintExpression (indent + 4) left
      ++ spaces (indent + 2)
      ++ "index:\n"
      ++ prettyPrintExpression (indent + 4) idx
  IfExpression _ condition consequence alternative ->
    spaces indent
      ++ "IfExpression\n"
      ++ spaces (indent + 2)
      ++ "condition:\n"
      ++ prettyPrintExpression (indent + 4) condition
      ++ spaces (indent + 2)
      ++ "consequence:\n"
      ++ intercalate "" (map (prettyPrintStatement (indent + 4)) consequence)
      ++ case alternative of
        Nothing -> ""
        Just alt ->
          spaces (indent + 2)
            ++ "alternative:\n"
            ++ intercalate "" (map (prettyPrintStatement (indent + 4)) alt)
  where
    spaces n = replicate n ' '
    renderParam (Param _ name ty _) = name ++ (\t -> ": " ++ show t) ty

expressionToString :: Expression -> String
expressionToString (IntLit _ val) = show val
expressionToString (FloatLit _ val) = show val
expressionToString (StringLit _ val) = val
expressionToString (IdentifierLit _ val) = val
expressionToString (BooleanLit _ val) = show val
expressionToString (ArrayLit _ elements) = "[" ++ intercalate ", " (map expressionToString elements) ++ "]"
expressionToString (PrefixExpression _ Bang val) = "(" ++ "!" ++ expressionToString val ++ ")"
expressionToString (PrefixExpression _ Minus val) = "(" ++ "-" ++ expressionToString val ++ ")"
expressionToString (InfixExpression _ left Plus right) = "(" ++ expressionToString left ++ " + " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Minus right) = "(" ++ expressionToString left ++ " - " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Asterisk right) = "(" ++ expressionToString left ++ " * " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Slash right) = "(" ++ expressionToString left ++ " / " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left LessThan right) = "(" ++ expressionToString left ++ " < " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left GreaterThan right) = "(" ++ expressionToString left ++ " > " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Equal right) = "(" ++ expressionToString left ++ " == " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left NotEqual right) = "(" ++ expressionToString left ++ " != " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left And right) = "(" ++ expressionToString left ++ " && " ++ expressionToString right ++ ")"
expressionToString (InfixExpression _ left Or right) = "(" ++ expressionToString left ++ " || " ++ expressionToString right ++ ")"

-- ============================================================
-- Typed AST
-- ============================================================

data TExpression
  = TIntLit {tToken :: Token, tIntValue :: Int, ty :: Type}
  | TFloatLit {tToken :: Token, tFloatValue :: Float, ty :: Type}
  | TStringLit {tToken :: Token, tStringValue :: String, ty :: Type}
  | TBoolLit {tToken :: Token, tBoolValue :: Bool, ty :: Type}
  | TNullLit {tToken :: Token, ty :: Type}
  | TIdentifierLit {tToken :: Token, tName :: String, tBinding :: Binding}
  | TPrefixExpression {tToken :: Token, operator :: Operator, right :: TExpression, ty :: Type}
  | TInfixExpression {tToken :: Token, left :: TExpression, operator :: Operator, right :: TExpression, ty :: Type}
  | TFunctionLit {tToken :: Token, parameters :: [TParam], returnType :: Type, body :: [TStatement], ty :: Type} -- Expression!
  | TCallExpression {tToken :: Token, function :: TExpression, arguments :: [TExpression], ty :: Type}
  | TIndexExpression {tToken :: Token, left :: TExpression, index :: TExpression, ty :: Type} -- This can only old and TIntLit as index
  | TIfExpression {tToken :: Token, condition :: TExpression, consequence :: Maybe TStatement, alternative :: Maybe TStatement, ty :: Type}
  | TFieldAccess {token :: Token, object :: TExpression, fieldName :: String, ty :: Type} -- Will typecheck iff the fieldname exists
  | TStructInitialization {token :: Token, structName :: String, fieldInits :: [TFieldInitialization], ty :: Type}
  deriving (Eq, Show)

instance HasType TExpression where
  getType = \case
    TIntLit {ty = t} -> t
    TFloatLit {ty = t} -> t
    TStringLit {ty = t} -> t
    TBoolLit {ty = t} -> t
    TNullLit {ty = t} -> t
    TIdentifierLit {tBinding = Binding _ t} -> t
    TPrefixExpression {ty = t} -> t
    TInfixExpression {ty = t} -> t
    TFunctionLit {ty = t} -> t
    TCallExpression {ty = t} -> t
    TIndexExpression {ty = t} -> t
    TIfExpression {ty = t} -> t
    TFieldAccess {ty = t} -> t
    TStructInitialization {ty = t} -> t

instance HasToken TExpression where
  getToken = \case
    TIntLit {tToken = tok} -> tok
    TFloatLit {tToken = tok} -> tok
    TStringLit {tToken = tok} -> tok
    TBoolLit {tToken = tok} -> tok
    TNullLit {tToken = tok} -> tok
    TIdentifierLit {tToken = tok} -> tok
    TPrefixExpression {tToken = tok} -> tok
    TInfixExpression {tToken = tok} -> tok
    TFunctionLit {tToken = tok} -> tok
    TCallExpression {tToken = tok} -> tok
    TIndexExpression {tToken = tok} -> tok
    TIfExpression {tToken = tok} -> tok
    TFieldAccess {token = tok} -> tok
    TStructInitialization {token = tok} -> tok

data TStatement
  = TProgram {tStatements :: [TStatement]}
  | TLetStatement {tLetToken :: Token, tLetName :: TExpression, tLetValue :: TExpression, tLetBinding :: Binding}
  | TAssignmentStatement {stmtToken :: Token, name :: TExpression, value :: TExpression, tAssBinding :: Binding}
  | TReturnStatement {tReturnToken :: Token, tResult :: TExpression}
  | TExpressionStatement {tStmtToken :: Token, tExpr :: TExpression}
  | TBlockStatement {tBlockStatements :: [TStatement]}
  | TStructDecl {tStmtToken :: Token, structName :: String, fields :: [FieldDecl]}
  deriving (Eq, Show)

data TParam = TParam
  { tParamToken :: Token,
    tParamName :: String,
    tParamType :: Type,
    tParamMut :: Mutability
  }
  deriving (Eq, Show)

class AstEq a where
  (?==) :: a -> a -> Bool

instance AstEq Expression where
  IntLit {intValue = l} ?== IntLit {intValue = r} = l == r
  FloatLit {floatValue = l} ?== FloatLit {floatValue = r} = l == r
  StringLit {stringValue = l} ?== StringLit {stringValue = r} = l == r
  IdentifierLit {name = l} ?== IdentifierLit {name = r} = l == r
  ArrayLit {elements = l} ?== ArrayLit {elements = r} = l ?== r
  BooleanLit {value = l} ?== BooleanLit {value = r} = l == r
  PrefixExpression {operator = lo, right = lr} ?== PrefixExpression {operator = ro, right = rr} =
    lo == ro && lr ?== rr
  InfixExpression {left = ll, operator = lo, right = lr} ?== InfixExpression {left = rl, operator = ro, right = rr} =
    lo == ro && ll ?== rl && lr ?== rr
  FunctionLit {parameters = lp, body = lb} ?== FunctionLit {parameters = rp, body = rb} =
    lp ?== rp && lb ?== rb
  CallExpression {function = lf, arguments = la} ?== CallExpression {function = rf, arguments = ra} =
    lf ?== rf && la ?== ra
  IndexExpression {left = ll, index = li} ?== IndexExpression {left = rl, index = ri} =
    ll ?== rl && li ?== ri
  IfExpression {condition = lc, consequence = lcons, alternative = lalt}
    ?== IfExpression {condition = rc, consequence = rcons, alternative = ralt} =
      lc ?== rc && lcons ?== rcons && lalt ?== ralt
  _ ?== _ = False

instance AstEq FieldDecl where
  FieldDecl _ n1 t1 ?== FieldDecl _ n2 t2 = n1 == n2 && t1 == t2

instance AstEq Param where
  Param _ n1 t1 m1 ?== Param _ n2 t2 m2 = n1 == n2 && t1 == t2 && m1 == m2

instance AstEq Statement where
  Program {statements = l} ?== Program {statements = r} = l ?== r
  LetStatement {name = ln, value = lv} ?== LetStatement {name = rn, value = rv} =
    ln ?== rn && lv ?== rv
  ReturnStatement {result = l} ?== ReturnStatement {result = r} = l ?== r
  ExpressionStatement {expr = l} ?== ExpressionStatement {expr = r} = l ?== r
  BlockStatement {statements = l} ?== BlockStatement {statements = r} = l ?== r
  StructDecl {structName = ln, fields = lf} ?== StructDecl {structName = rn, fields = rf} =
    ln == rn && lf ?== rf
  _ ?== _ = False

-- lists for free
instance (AstEq a) => AstEq [a] where
  [] ?== [] = True
  (x : xs) ?== (y : ys) = x ?== y && xs ?== ys
  _ ?== _ = False

-- maybe for free
instance (AstEq a) => AstEq (Maybe a) where
  Nothing ?== Nothing = True
  Just l ?== Just r = l ?== r
  _ ?== _ = False
