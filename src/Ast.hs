{-# LANGUAGE DuplicateRecordFields #-}

{- HLINT ignore "Used otherwise as a pattern" -}

module Ast (Expression (..), Statement (..), showExprTree, showTree, infixToPrecedence) where

import Token (Token, TokenType (..), tokenType)

type Operator = TokenType

data Expression
  = IntLit {token :: Token, intValue :: Integer}
  | FloatLit {token :: Token, floatValue :: Float}
  | StringLit {token :: Token, stringValue :: String}
  | IdentifierLit {token :: Token, name :: String}
  | BooleanLit {token :: Token, value :: Bool}
  | PrefixExpression {token :: Token, operator :: Operator, right :: Expression}
  | InfixExpression {token :: Token, left :: Expression, operator :: Operator, right :: Expression}
  | FunctionLit {token :: Token, parameters :: [Expression], body :: [Statement]} -- Expression!
  | CallExpression {token :: Token, function :: Expression, arguments :: [Expression]}
  | IndexExpression {token :: Token, left :: Expression, index :: Expression}
  | ArrayLit {token :: Token, elements :: [Expression]}
  deriving (Eq, Show)

data Statement
  = Program {statements :: [Statement]}
  | LetStatement {token :: Token, name :: Expression, value :: Expression} -- Expression!
  | ReturnStatement {token :: Token, result :: Expression}
  | ExpressionStatement {token :: Token, expr :: Expression}
  | BlockStatement {statements :: [Statement]}
  | IfStatement {token :: Token, condition :: Expression, consequence :: [Statement], alternative :: Maybe [Statement]}
  deriving (Eq, Show)

infixToPrecedence :: TokenType -> Integer
infixToPrecedence token = case token of
  Equal -> 1
  NotEqual -> 10
  LessThan -> 10
  GreaterThan -> 10
  Plus -> 10
  Minus -> 10
  Slash -> 15
  Asterisk -> 15
  LParen -> 10
  LBracket -> 10
  otherwise -> 0

-------------------------
--
--
showTree :: Statement -> String
showTree stmt = unlines $ go stmt
  where
    indent n = replicate (n * 2) ' '

    -- Statements
    go (Program stmts) =
      "Program" : concatMap (map ("  " ++) . go) stmts
    go (LetStatement _ name val) =
      "LetStatement"
        : map ("├─name: " ++) (goExpr name)
        ++ map ("└─value: " ++) (goExpr val)
    go (ReturnStatement _ result) =
      "ReturnStatement"
        : map ("└─ " ++) (goExpr result)
    go (ExpressionStatement _ expr) =
      "ExpressionStatement"
        : map ("└─ " ++) (goExpr expr)
    go (BlockStatement stmts) =
      "BlockStatement" : concatMap (map ("  " ++) . go) stmts
    go (IfStatement _ cond conseq alt) =
      "IfStatement"
        : map ("├─condition: " ++) (goExpr cond)
        ++ ("├─consequence:" : concatMap (map ("│   " ++) . go) conseq)
        ++ case alt of
          Nothing -> ["└─alternative: None"]
          Just a -> "└─alternative:" : concatMap (map ("    " ++) . go) a

    -- Expressions
    goExpr (IntLit _ val) = ["IntLit " ++ show val]
    goExpr (FloatLit _ val) = ["FloatLit " ++ show val]
    goExpr (StringLit _ val) = ["StringLit " ++ show val]
    goExpr (IdentifierLit _ name) = ["Identifier " ++ name]
    goExpr (BooleanLit _ val) = ["Boolean " ++ show val]
    goExpr (PrefixExpression _ op right) =
      ("PrefixExpression (" ++ show op ++ ")")
        : map ("└─ " ++) (goExpr right)
    goExpr (InfixExpression _ left op right) =
      ("InfixExpression (" ++ show op ++ ")")
        : map ("├─ " ++) (goExpr left)
        ++ map ("└─ " ++) (goExpr right)
    goExpr (FunctionLit _ params body) =
      "FunctionLit"
        : ("├─parameters: " ++ show (length params))
        : concatMap (map ("│   " ++) . goExpr) params
        ++ ("└─body:" : concatMap (map ("    " ++) . go) body)
    goExpr (CallExpression _ func args) =
      "CallExpression"
        : map ("├─function: " ++) (goExpr func)
        ++ if null args
          then ["└─arguments: None"]
          else ("└─arguments:" : concatMap (map ("    " ++) . goExpr) args)
    goExpr (IndexExpression _ left idx) =
      "IndexExpression"
        : map ("├─left: " ++) (goExpr left)
        ++ map ("└─index: " ++) (goExpr idx)
    goExpr (ArrayLit _ elems) =
      ("ArrayLit [" ++ show (length elems) ++ " elements]")
        : if null elems
          then []
          else concatMap (map ("  " ++) . goExpr) elems

showExprTree :: Expression -> String
showExprTree expr = unlines $ goExpr "" "" expr
  where
    -- goExpr prefix childPrefix expression
    goExpr prefix childPrefix (IntLit _ val) =
      [prefix ++ "IntLit " ++ show val]
    goExpr prefix childPrefix (FloatLit _ val) =
      [prefix ++ "FloatLit " ++ show val]
    goExpr prefix childPrefix (StringLit _ val) =
      [prefix ++ "StringLit " ++ show val]
    goExpr prefix childPrefix (IdentifierLit _ name) =
      [prefix ++ "Identifier " ++ name]
    goExpr prefix childPrefix (BooleanLit _ val) =
      [prefix ++ "Boolean " ++ show val]
    goExpr prefix childPrefix (PrefixExpression _ op right) =
      (prefix ++ "PrefixExpression (" ++ show op ++ ")")
        : goExpr (childPrefix ++ "└─ ") (childPrefix ++ "   ") right
    goExpr prefix childPrefix (InfixExpression _ left op right) =
      (prefix ++ "InfixExpression (" ++ show op ++ ")")
        : goExpr (childPrefix ++ "├─ ") (childPrefix ++ "│  ") left
        ++ goExpr (childPrefix ++ "└─ ") (childPrefix ++ "   ") right
    goExpr prefix childPrefix (ArrayLit _ elems) =
      (prefix ++ "ArrayLit [" ++ show (length elems) ++ " elements]")
        : concatMap (\e -> goExpr (childPrefix ++ "├─ ") (childPrefix ++ "│  ") e) (init' elems)
        ++ maybe [] (\e -> goExpr (childPrefix ++ "└─ ") (childPrefix ++ "   ") e) (lastMaybe elems)
      where
        init' [] = []
        init' xs = init xs
        lastMaybe [] = Nothing
        lastMaybe xs = Just (last xs)
    goExpr prefix childPrefix (FunctionLit _ params body) =
      (prefix ++ "FunctionLit")
        : [childPrefix ++ "├─ parameters: [" ++ show (length params) ++ "]"]
        ++ [childPrefix ++ "└─ body: [" ++ show (length body) ++ " statements]"]
    goExpr prefix childPrefix (CallExpression _ func args) =
      (prefix ++ "CallExpression")
        : goExpr (childPrefix ++ "├─ function: ") (childPrefix ++ "│  ") func
        ++ [childPrefix ++ "└─ arguments: [" ++ show (length args) ++ "]"]
    goExpr prefix childPrefix (IndexExpression _ left idx) =
      (prefix ++ "IndexExpression")
        : goExpr (childPrefix ++ "├─ ") (childPrefix ++ "│  ") left
        ++ goExpr (childPrefix ++ "└─ index: ") (childPrefix ++ "   ") idx
