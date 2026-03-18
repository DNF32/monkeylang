module Eval where

import Ast (Expression (..), Param (..), Statement (..), expressionToString)
import Control.Lens
import Control.Monad.State
import Data.List (intercalate)
import Data.Map qualified as Map
import Parser (AstParserError (..), ParserError (..), parseProgram, runAstParser)
import Token
import TypeChecker

emptyMap :: Map.Map k v
emptyMap = Map.empty

-- Safe version
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

type Enviroment = Map.Map String Object

type Eval a = State Enviroment a

-- Errors track position
data EvalError
  = TypeError {message :: String, pos :: Maybe Position}
  | UndefinedVariable {varName :: String, pos :: Maybe Position}
  | DivisionByZero {pos :: Maybe Position}
  | ParseError {message :: String, pos :: Maybe Position} -- NEW
  | InvalidFunctionParameter {pos :: Maybe Position}
  | InvalidFunctionCall {pos :: Maybe Position}
  | UndefinedFunction {varName :: String, pos :: Maybe Position}
  | InvalidIndexTarget {got :: String, pos :: Maybe Position} -- indexing a non-array
  | InvalidIndexType {expected :: String, got :: String, pos :: Maybe Position}
  | IndexOutOfBounds {outOfBoundsIndex :: Int, arrayLength :: Int, pos :: Maybe Position}
  deriving (Show, Eq)

withPos :: Position -> Object -> Object
withPos p (ErrorObj err) = ErrorObj (err {pos = Just p})
withPos _ obj = obj

data Object
  = IntObj Int
  | BoolObj Bool
  | FloatObj Float
  | StringObj String
  | FunctionObj [Param] [Statement] Enviroment
  | NullObj
  | ArrayObj [Object]
  | ReturnObj Object
  | ErrorObj EvalError
  | VoidObj
  deriving (Show, Eq)

interpreter :: String -> Object
interpreter program =
  let initialLexerState = initialState program
   in case runAstParser parseProgram initialLexerState of
        Right (prog, _) -> fst $ runState (evalProgram prog) newEnv
        Left parseErr -> ErrorObj (astParserErrorToEvalError parseErr)

--
-- Convert AstParserError to EvalError
-- INFO: Why do we need this ? Because the errors will bubble up?
astParserErrorToEvalError :: AstParserError -> EvalError
astParserErrorToEvalError (TokenError (LexError msg pos)) = ParseError msg (Just pos)
astParserErrorToEvalError (SyntaxError (ParserError msg pos)) = ParseError msg (Just pos)

objectToString :: Object -> String
objectToString (IntObj v) = show v
objectToString (BoolObj v) = if v then "true" else "false"
objectToString (FloatObj v) = show v
objectToString (StringObj v) = "\"" ++ v ++ "\""
objectToString (FunctionObj params _ _) = "fn(" ++ show (length params) ++ " params)"
objectToString (ReturnObj obj) = objectToString obj
objectToString NullObj = "null"
objectToString (ArrayObj elems) = "[" ++ intercalate ", " (map objectToString elems) ++ "]"
objectToString (ErrorObj err) = "ERROR: " ++ show err

objectTypeString :: Object -> String
objectTypeString (IntObj _) = "Integer"
objectTypeString (BoolObj _) = "Boolean"
objectTypeString (FloatObj _) = "Float"
objectTypeString (StringObj _) = "String"
objectTypeString (ArrayObj _) = "Array"
objectTypeString (FunctionObj {}) = "Function"
objectTypeString NullObj = "Null"
objectTypeString (ReturnObj _) = "Return"
objectTypeString (ErrorObj _) = "Error"

falseObj :: Object
falseObj = BoolObj False

trueObj :: Object
trueObj = BoolObj True

nullObj :: Object
nullObj = NullObj

voidObj :: Object
voidObj = VoidObj

hasIdent :: String -> Eval Bool
hasIdent name = gets (Map.member name)

setObject :: String -> Object -> Eval ()
setObject name obj = modify (Map.insert name obj)

getObject :: String -> Eval (Maybe Object)
getObject name = gets (Map.lookup name)

newEnv :: Enviroment
newEnv = Map.empty

initialEnv :: Eval ()
initialEnv = put newEnv

evalProgram :: Statement -> Eval Object
evalProgram (Program stmts) = evalProgram' stmts
evalProgram _ = undefined

evalProgram' :: [Statement] -> Eval Object
evalProgram' [] = return voidObj
evalProgram' [s] = case s of
  LetStatement {} -> evalLetStatement s
  ReturnStatement {} -> evalReturnStatement s
  ExpressionStatement {} -> do
    obj <- evalExpressionStatement s
    case obj of
      ReturnObj wrappedValue -> return wrappedValue
      _ -> return obj
  _ -> return voidObj
evalProgram' (s1 : ss) = do
  case s1 of
    LetStatement {} -> do
      _ <- evalLetStatement s1
      evalProgram' ss
    ReturnStatement {} -> evalReturnStatement s1
    ExpressionStatement {} -> do
      obj <- evalExpressionStatement s1
      case obj of
        ReturnObj wrappedValue -> return wrappedValue
        ErrorObj _ -> return obj
        _ -> evalProgram' ss
    _ -> return voidObj

evalBlockStatement :: [Statement] -> Eval Object
evalBlockStatement [] = return voidObj
evalBlockStatement [s] = case s of
  LetStatement {} -> evalLetStatement s
  ReturnStatement {} -> evalReturnStatement s
  ExpressionStatement {} -> do
    obj <- evalExpressionStatement s
    case obj of
      ReturnObj _ -> return obj
      ErrorObj _ -> return obj
      _ -> return voidObj
  _ -> return voidObj
evalBlockStatement (s1 : ss) = do
  case s1 of
    LetStatement {} -> do
      _ <- evalLetStatement s1
      evalBlockStatement ss
    ReturnStatement {} -> evalReturnStatement s1
    ExpressionStatement {} -> do
      obj <- evalExpressionStatement s1
      case obj of
        ReturnObj _ -> return obj
        ErrorObj _ -> return obj
        _ -> evalBlockStatement ss
    _ -> return voidObj

evalLetStatement :: Statement -> Eval Object
evalLetStatement (LetStatement _ (IdentifierLit _ name) value _) = do
  obj <- unwrapReturnObj <$> evalExpression value
  case obj of
    ErrorObj _ -> return obj
    FunctionObj params body capturedEnv -> do
      let selfEnv = Map.insert name (FunctionObj params body selfEnv) capturedEnv
          updatedObj = FunctionObj params body selfEnv
      setObject name updatedObj
      return updatedObj
    _ -> do
      setObject name obj
      return obj
evalLetStatement s = error $ "evalLetStatement called with non-LetStatement: " ++ show s

evalReturnStatement :: Statement -> Eval Object
evalReturnStatement (ReturnStatement _ expr) = do
  obj <- evalExpression expr
  case obj of
    ReturnObj _ -> return obj
    ErrorObj _ -> return obj
    _ -> return (ReturnObj obj)
evalReturnStatement s = error $ "evalReturnStatement called with non-ReturnStatement: " ++ show s

evalExpressionStatement :: Statement -> Eval Object
evalExpressionStatement (ExpressionStatement _ expr) = evalExpression expr
evalExpressionStatement s = error $ "evalExpressionStatement called with non-ExpressionStatement: " ++ show s

evalExpression :: Expression -> Eval Object
evalExpression (IntLit _ intValue) = return (IntObj (fromIntegral intValue))
evalExpression (FloatLit _ floatValue) = return (FloatObj floatValue)
evalExpression (StringLit _ stringValue) = return (StringObj stringValue)
evalExpression (BooleanLit _ value) =
  return (if value then trueObj else falseObj)
evalExpression (IdentifierLit (Token _ pos) name) = do
  maybeObj <- getObject name
  case maybeObj of
    Just obj -> return obj
    Nothing -> return (ErrorObj (UndefinedVariable name (Just pos)))
evalExpression (IfExpression _ condition consequence alternative) = do
  conditionObj <- evalExpression condition
  case isTruthy conditionObj of
    True -> evalBlockStatement consequence
    False -> case alternative of
      Just stmts -> evalBlockStatement stmts
      Nothing -> return voidObj
evalExpression (PrefixExpression (Token _ pos) operator right) = do
  rightSide <- unwrapReturnObj <$> evalExpression right
  return $ case operator of
    Bang -> evalBang rightSide
    Minus -> evalMinus rightSide
    _ -> addPos $ ErrorObj (TypeError ("Prefix operation not supported: " ++ expressionToString right) Nothing)
  where
    addPos obj = case obj of
      ErrorObj _ -> withPos pos obj
      _ -> obj

    evalBang (IntObj v) = if v == 0 then trueObj else falseObj
    evalBang (FloatObj _) = trueObj
    evalBang (StringObj v) = if v == "" then trueObj else falseObj
    evalBang (BoolObj v) = if v then falseObj else trueObj
    evalBang o = addPos $ ErrorObj (TypeError ("Bang operator not supported for: " ++ expressionToString right) Nothing)

    evalMinus (IntObj v) = IntObj (-v)
    evalMinus (FloatObj v) = FloatObj (-v)
    evalMinus o = addPos $ ErrorObj (TypeError ("Minus operator not supported for: " ++ expressionToString right) Nothing)
evalExpression (InfixExpression (Token _ pos) left operator right) = do
  leftSide <- unwrapReturnObj <$> evalExpression left
  rightSide <- unwrapReturnObj <$> evalExpression right

  return $ case operator of
    Plus -> addPos $ evalSum leftSide rightSide
    Minus -> addPos $ evalMinus leftSide rightSide
    Asterisk -> addPos $ evalProduct leftSide rightSide
    Slash -> addPos $ evalDivide leftSide rightSide
    Equal -> addPos $ evalEquals leftSide rightSide
    NotEqual -> addPos $ evalNotEquals leftSide rightSide
    LessThan -> addPos $ evalLessThan leftSide rightSide
    GreaterThan -> addPos $ evalGreaterThan leftSide rightSide
  where
    addPos obj = case obj of
      ErrorObj _ -> withPos pos obj
      _ -> obj
evalExpression (FunctionLit (Token _ pos) parameters _ body) = do
  env <- get
  return (FunctionObj parameters body env)
evalExpression (CallExpression (Token _ pos) (IdentifierLit _ funcName) args) = do
  maybeFunc <- getObject funcName
  case maybeFunc of
    Just (FunctionObj params body env) -> do
      functionEval params body env args
    _ -> do
      return (ErrorObj (UndefinedFunction funcName (Just pos)))
evalExpression (CallExpression (Token _ pos) (FunctionLit _ params _ body) args) = do
  oldEnv <- get
  functionEval params body oldEnv args
evalExpression (CallExpression (Token _ pos) call@(CallExpression _ _ _) outerArgs) = do
  maybeFunc <- evalExpression call
  case maybeFunc of
    FunctionObj params body env -> functionEval params body env outerArgs
    ReturnObj (FunctionObj params body env) -> functionEval params body env outerArgs
    _ -> return voidObj
evalExpression (ArrayLit _ elms) = do
  evalExpr <- mapM evalExpression elms
  return (ArrayObj evalExpr)
evalExpression (IndexExpression (Token _ pos) left indexExpr) = do
  maybeArrayObj <- evalExpression left
  case maybeArrayObj of
    ErrorObj _ -> return maybeArrayObj
    ArrayObj list -> do
      evalIndex <- evalExpression indexExpr
      case evalIndex of
        ErrorObj _ -> return evalIndex
        IntObj idx ->
          case safeIndex list (fromIntegral idx) of
            Just value -> return value
            Nothing ->
              return
                ( ErrorObj
                    ( IndexOutOfBounds
                        { outOfBoundsIndex = fromIntegral idx,
                          arrayLength = length list,
                          pos = Just pos
                        }
                    )
                )
        _ ->
          return
            ( ErrorObj
                ( InvalidIndexType
                    { expected = "Integer",
                      got = objectTypeString evalIndex,
                      pos = Just pos
                    }
                )
            )
    _ ->
      return
        ( ErrorObj
            ( InvalidIndexTarget
                { got = objectTypeString maybeArrayObj,
                  pos = Just pos
                }
            )
        )
evalExpression expr =
  let tok = (token :: Expression -> Token) expr
   in return
        ( ErrorObj
            ( TypeError
                ("Unhandled expression: " ++ show expr)
                (Just (_tokenPosition tok))
            )
        )

functionEval :: [Param] -> [Statement] -> Enviroment -> [Expression] -> Eval Object
functionEval params body env args
  | length params /= length args = return (ErrorObj (InvalidFunctionCall Nothing))
  | otherwise = do
      evaluatedArgs <- map unwrapReturnObj <$> mapM evalExpression args
      if any isError evaluatedArgs
        then return (head (filter isError evaluatedArgs))
        else do
          oldEnv <- get
          put (extendedEnv env params evaluatedArgs)
          result <- evalBlockStatement body
          put oldEnv
          return result

extendedEnv :: Enviroment -> [Param] -> [Object] -> Enviroment
extendedEnv env params evaluatedArgs =
  Map.union (Map.fromList [(name, obj) | (Param _ name _, obj) <- zip params evaluatedArgs]) env

isError :: Object -> Bool
isError (ErrorObj _) = True
isError _ = False

unwrapReturnObj :: Object -> Object
unwrapReturnObj (ReturnObj obj) = obj
unwrapReturnObj obj = obj

evalSum :: Object -> Object -> Object
evalSum left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 + v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 + v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 + v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 + fromIntegral v2)
  (StringObj v1, StringObj v2) -> StringObj (v1 ++ v2)
  (_, _) -> ErrorObj (TypeError ("Sum operator doesn't support: " ++ objectToString left ++ " + " ++ objectToString right) Nothing)

evalMinus :: Object -> Object -> Object
evalMinus left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 - v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 - v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 - v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 - fromIntegral v2)
  (_, _) -> ErrorObj (TypeError ("Minus operator doesn't support: " ++ objectToString left ++ " - " ++ objectToString right) Nothing)

evalProduct :: Object -> Object -> Object
evalProduct left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 * v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 * v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 * v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 * fromIntegral v2)
  (_, _) -> ErrorObj (TypeError ("Product operator doesn't support: " ++ objectToString left ++ " * " ++ objectToString right) Nothing)

evalDivide :: Object -> Object -> Object
evalDivide left right = case (left, right) of
  (_, IntObj 0) -> ErrorObj (DivisionByZero Nothing)
  (_, FloatObj 0.0) -> ErrorObj (DivisionByZero Nothing)
  (IntObj v1, IntObj v2) -> IntObj (v1 `div` v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 / v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 / v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 / fromIntegral v2)
  (_, _) -> ErrorObj (TypeError ("Division operator doesn't support: " ++ objectToString left ++ " / " ++ objectToString right) Nothing)

evalEquals :: Object -> Object -> Object
evalEquals left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 == v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 == v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 == v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 == fromIntegral v2 then trueObj else falseObj
  (StringObj v1, StringObj v2) -> if v1 == v2 then trueObj else falseObj
  (BoolObj v1, BoolObj v2) -> if v1 == v2 then trueObj else falseObj
  (NullObj, NullObj) -> trueObj
  _ -> ErrorObj (TypeError ("Equals failed: equality comparison returned non-boolean " ++ objectToString left ++ " == " ++ objectToString right) Nothing)

evalNotEquals :: Object -> Object -> Object
evalNotEquals left right = case evalEquals left right of
  BoolObj True -> falseObj
  BoolObj False -> trueObj
  ErrorObj _ -> ErrorObj (TypeError "NotEquals failed: equality comparison returned non-boolean" Nothing)

evalLessThan :: Object -> Object -> Object
evalLessThan left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 < v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 < v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 < v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 < fromIntegral v2 then trueObj else falseObj
  (_, _) -> ErrorObj (TypeError ("LessThan operator only supports numbers: " ++ objectToString left ++ " < " ++ objectToString right) Nothing)

evalGreaterThan :: Object -> Object -> Object
evalGreaterThan left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 > v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 > v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 > v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 > fromIntegral v2 then trueObj else falseObj
  (_, _) -> ErrorObj (TypeError ("GreaterThan operator only supports numbers: " ++ objectToString left ++ " > " ++ objectToString right) Nothing)

isTruthy :: Object -> Bool
isTruthy obj = case obj of
  NullObj -> False
  BoolObj True -> True
  BoolObj False -> False
  IntObj v1 -> v1 /= 0
  _ -> False
