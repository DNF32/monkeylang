module Eval where

import Ast (Expression (..), Statement (..), exprToken, expressionToString)
import Control.Lens
import Control.Monad.State
import Data.Map qualified as Map
import Debug.Trace
import Parser (AstParserError (..), ParserError (..), parseProgram, runAstParser)
import Token

emptyMap :: Map.Map k v
emptyMap = Map.empty

type Enviroment = Map.Map String Object

type Eval a = State Enviroment a

-- Errors track position
data EvalError
  = TypeError {errorPos :: Position, message :: String}
  | UndefinedVariable {errorPos :: Position, varName :: String}
  | DivisionByZero {errorPos :: Position}
  | ParseError {errorPos :: Position, message :: String} -- NEW
  | InvalidFunctionParameter {errorPos :: Position}
  | InvalidFunctionCall {errorPos :: Position}
  deriving (Show, Eq)

data Object
  = IntObj Integer
  | BoolObj Bool
  | FloatObj Float
  | StringObj String
  | FunctionObj [Expression] [Statement] Enviroment
  | NullObj
  | ReturnObj Object
  | ErrorObj EvalError
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
astParserErrorToEvalError (TokenError (LexError msg pos)) =
  ParseError pos msg
astParserErrorToEvalError (SyntaxError (ParserError msg pos)) =
  ParseError pos msg

objectToString :: Object -> String
objectToString (IntObj v) = show v
objectToString (BoolObj v) = if v then "true" else "false"
objectToString (FloatObj v) = show v
objectToString (StringObj v) = "\"" ++ v ++ "\""
objectToString (FunctionObj params _ _) = "fn(" ++ show (length params) ++ " params)"
objectToString (ReturnObj obj) = objectToString obj
objectToString NullObj = "null"
objectToString (ErrorObj err) = "ERROR: " ++ show err

falseObj :: Object
falseObj = BoolObj False

trueObj :: Object
trueObj = BoolObj True

nullObj :: Object
nullObj = NullObj

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
evalProgram' [] = return nullObj
evalProgram' [s] = case s of
  LetStatement {} -> evalLetStatement s
  ReturnStatement {} -> evalReturnStatement s
  ExpressionStatement {} -> do
    obj <- evalExpressionStatement s
    case obj of
      ReturnObj wrappedValue -> return wrappedValue
      _ -> return obj
  _ -> return nullObj
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
    _ -> return nullObj

evalBlockStatement :: [Statement] -> Eval Object
evalBlockStatement [] = return nullObj
evalBlockStatement [s] = case s of
  LetStatement {} -> evalLetStatement s
  ReturnStatement {} -> evalReturnStatement s
  ExpressionStatement {} -> do
    obj <- evalExpressionStatement s
    case obj of
      ReturnObj _ -> return obj
      ErrorObj _ -> return obj
      _ -> return nullObj
  _ -> return nullObj
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
    _ -> return nullObj

evalLetStatement :: Statement -> Eval Object
evalLetStatement (LetStatement _ (IdentifierLit _ name) value) = do
  obj <- evalExpression value
  trace ("LetStatement: evaluated " ++ name ++ " = " ++ objectToString obj) $ do
    case obj of
      ReturnObj wrappedValue -> do
        setObject name wrappedValue
        return (ReturnObj wrappedValue)
      ErrorObj _ -> return obj
      _ -> do
        setObject name obj
        trace ("LetStatement: bound " ++ name ++ " = " ++ objectToString obj) $
          case obj of
            FunctionObj params body capturedEnv -> do
              let selfEnv = Map.insert name (FunctionObj params body selfEnv) capturedEnv
              trace ("LetStatement: selfEnv has " ++ name ++ "? " ++ show (Map.member name selfEnv)) $ do
                let updatedObj = FunctionObj params body selfEnv
                setObject name updatedObj
                return updatedObj
            _ -> return obj

evalReturnStatement :: Statement -> Eval Object
evalReturnStatement (ReturnStatement _ expr) = do
  obj <- evalExpression expr
  case obj of
    ReturnObj _ -> return obj
    ErrorObj _ -> return obj
    _ -> return (ReturnObj obj)

evalExpressionStatement :: Statement -> Eval Object
evalExpressionStatement (ExpressionStatement _ expr) = evalExpression expr

evalExpression :: Expression -> Eval Object
evalExpression (IntLit _ intValue) = return (IntObj intValue)
evalExpression (FloatLit _ floatValue) = return (FloatObj floatValue)
evalExpression (StringLit _ stringValue) = return (StringObj stringValue)
evalExpression (BooleanLit _ value) =
  return (if value then trueObj else falseObj)
evalExpression (IdentifierLit tok name) = do
  maybeObj <- getObject name
  case maybeObj of
    Just obj -> return obj
    Nothing -> return (ErrorObj (UndefinedVariable (tok ^. tokenPosition) name))
evalExpression (IfExpression tok condition consequence alternative) = do
  conditionObj <- evalExpression condition
  obj <- case isTruthy conditionObj of
    True -> evalBlockStatement consequence
    False -> case alternative of
      Just stmts -> evalBlockStatement stmts
      Nothing -> return nullObj
  case obj of
    ReturnObj _ -> return obj
    ErrorObj _ -> return obj
    _ -> return obj
evalExpression (PrefixExpression _ operator right) = do
  rightSide <- evalExpression right
  let obj = case (operator, rightSide) of
        (Bang, IntObj value) -> if value == 0 then trueObj else falseObj
        (Bang, FloatObj _) -> trueObj
        (Bang, StringObj value) -> if value == "" then trueObj else falseObj
        (Bang, BoolObj v1) -> if v1 then falseObj else trueObj
        (Minus, IntObj value) -> IntObj (-value)
        (Minus, FloatObj value) -> FloatObj (-value)
        (_, _) -> ErrorObj (TypeError (right ^. exprToken . tokenPosition) ("Prefix operation not supported, value at" ++ expressionToString right ++ "not supported"))
  return obj
evalExpression (InfixExpression tok left operator right) = do
  leftSide <- evalExpression left
  rightSide <- evalExpression right

  let unwrap (ReturnObj o) = o
      unwrap o = o
  let leftSide' = unwrap leftSide
      rightSide' = unwrap rightSide

  let pos = tok ^. tokenPosition -- Get position from the infix expression itself
  let obj = case operator of
        Plus -> evalSum pos leftSide' rightSide'
        Minus -> evalMinus pos leftSide' rightSide'
        Asterisk -> evalProduct pos leftSide' rightSide'
        Slash -> evalDivide pos leftSide' rightSide'
        Equal -> evalEquals pos leftSide' rightSide'
        NotEqual -> evalNotEquals pos leftSide' rightSide'
        LessThan -> evalLessThan pos leftSide' rightSide'
        GreaterThan -> evalGreaterThan pos leftSide' rightSide'
  return obj
evalExpression (FunctionLit tok parameters body) = do
  if all isIdentifierLit parameters
    then do
      env <- get
      return (FunctionObj parameters body env)
    else
      return (ErrorObj (InvalidFunctionParameter (tok ^. tokenPosition)))
evalExpression (CallExpression tok (IdentifierLit _ funcName) args) = do
  maybeFunc <- getObject funcName
  case maybeFunc of
    Just (FunctionObj params body env) -> do
      if length params /= length args
        then return (ErrorObj (InvalidFunctionCall (tok ^. tokenPosition)))
        else do
          evaluatedArgs <- mapM evalExpression args
          let unwrapedArgs = map unwrapReturnObj evaluatedArgs
          if any isError evaluatedArgs
            then return (head (filter isError unwrapedArgs))
            else do
              let extendedEnv = Map.union (Map.fromList [(name, obj) | (IdentifierLit _ name, obj) <- zip params unwrapedArgs]) env
              oldEnv <- get
              put extendedEnv
              result <- evalBlockStatement body
              put oldEnv
              return result
    _ -> do
      return (ErrorObj (InvalidFunctionParameter (tok ^. tokenPosition)))
evalExpression (CallExpression tok (FunctionLit _ params body) args) = do
  if length params /= length args
    then return (ErrorObj (InvalidFunctionCall (tok ^. tokenPosition)))
    else do
      evaluatedArgs <- mapM evalExpression args
      if any isError evaluatedArgs
        then return (head (filter isError evaluatedArgs))
        else do
          oldEnv <- get
          let extendedEnv = Map.union (Map.fromList [(name, obj) | (IdentifierLit _ name, obj) <- zip params evaluatedArgs]) oldEnv
          put extendedEnv
          result <- evalBlockStatement body
          put oldEnv
          return result
evalExpression (CallExpression tok call@(CallExpression _ _ _) outerArgs) = do
  maybeFunc <- evalExpression call
  case maybeFunc of
    FunctionObj params body env -> do
      if length params /= length outerArgs
        then return (ErrorObj (InvalidFunctionCall (tok ^. tokenPosition)))
        else do
          evaluatedArgs <- mapM evalExpression outerArgs
          let unwrapedArgs = map unwrapReturnObj evaluatedArgs
          if any isError unwrapedArgs
            then return (head (filter isError unwrapedArgs))
            else do
              let extendedEnv = Map.union (Map.fromList [(name, obj) | (IdentifierLit _ name, obj) <- zip params unwrapedArgs]) env
              oldEnv <- get
              put extendedEnv
              result <- evalBlockStatement body
              put oldEnv
              return result
    ReturnObj (FunctionObj params body env) -> do
      if length params /= length outerArgs
        then return (ErrorObj (InvalidFunctionCall (tok ^. tokenPosition)))
        else do
          evaluatedArgs <- mapM evalExpression outerArgs
          let unwrapedArgs = map unwrapReturnObj evaluatedArgs
          if any isError unwrapedArgs
            then return (head (filter isError unwrapedArgs))
            else do
              let extendedEnv = Map.union (Map.fromList [(name, obj) | (IdentifierLit _ name, obj) <- zip params unwrapedArgs]) env
              oldEnv <- get
              put extendedEnv
              result <- evalBlockStatement body
              put oldEnv
              return result
    _ -> return nullObj
evalExpression expr = return (ErrorObj (TypeError (expr ^. exprToken . tokenPosition) ("Unhandled expression: " ++ show (expr))))

isError :: Object -> Bool
isError (ErrorObj _) = True
isError _ = False

unwrapReturnObj :: Object -> Object
unwrapReturnObj (ReturnObj obj) = obj
unwrapReturnObj obj = obj

isIdentifierLit :: Expression -> Bool
isIdentifierLit (IdentifierLit _ _) = True
isIdentifierLit _ = False

evalSum :: Position -> Object -> Object -> Object
evalSum pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 + v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 + v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 + v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 + fromIntegral v2)
  (StringObj v1, StringObj v2) -> StringObj (v1 ++ v2)
  (_, _) -> ErrorObj (TypeError pos ("Sum operator doesn't support: " ++ objectToString left ++ " + " ++ objectToString right))

evalMinus :: Position -> Object -> Object -> Object
evalMinus pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 - v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 - v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 - v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 - fromIntegral v2)
  (_, _) -> ErrorObj (TypeError pos ("Minus operator doesn't support: " ++ objectToString left ++ " - " ++ objectToString right))

evalProduct :: Position -> Object -> Object -> Object
evalProduct pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> IntObj (v1 * v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 * v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 * v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 * fromIntegral v2)
  (_, _) -> ErrorObj (TypeError pos ("Product operator doesn't support: " ++ objectToString left ++ " * " ++ objectToString right))

evalDivide :: Position -> Object -> Object -> Object
evalDivide pos left right = case (left, right) of
  (_, IntObj 0) -> ErrorObj (DivisionByZero pos)
  (_, FloatObj 0.0) -> ErrorObj (DivisionByZero pos)
  (IntObj v1, IntObj v2) -> IntObj (v1 `div` v2)
  (FloatObj v1, FloatObj v2) -> FloatObj (v1 / v2)
  (IntObj v1, FloatObj v2) -> FloatObj (fromIntegral v1 / v2)
  (FloatObj v1, IntObj v2) -> FloatObj (v1 / fromIntegral v2)
  (_, _) -> ErrorObj (TypeError pos ("Division operator doesn't support: " ++ objectToString left ++ " / " ++ objectToString right))

evalEquals :: Position -> Object -> Object -> Object
evalEquals pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 == v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 == v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 == v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 == fromIntegral v2 then trueObj else falseObj
  (StringObj v1, StringObj v2) -> if v1 == v2 then trueObj else falseObj
  (BoolObj v1, BoolObj v2) -> if v1 == v2 then trueObj else falseObj
  (NullObj, NullObj) -> trueObj
  _ -> ErrorObj (TypeError pos ("Equals failed: equality comparison returned non-boolean " ++ objectToString left ++ " == " ++ objectToString right))

evalNotEquals :: Position -> Object -> Object -> Object
evalNotEquals pos left right = case evalEquals pos left right of
  BoolObj True -> falseObj
  BoolObj False -> trueObj
  ErrorObj m -> ErrorObj (TypeError pos "NotEquals failed: equality comparison returned non-boolean")

evalLessThan :: Position -> Object -> Object -> Object
evalLessThan pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 < v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 < v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 < v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 < fromIntegral v2 then trueObj else falseObj
  (_, _) -> ErrorObj (TypeError pos ("LessThan operator only supports numbers: " ++ objectToString left ++ " < " ++ objectToString right))

evalGreaterThan :: Position -> Object -> Object -> Object
evalGreaterThan pos left right = case (left, right) of
  (IntObj v1, IntObj v2) -> if v1 > v2 then trueObj else falseObj
  (FloatObj v1, FloatObj v2) -> if v1 > v2 then trueObj else falseObj
  (IntObj v1, FloatObj v2) -> if fromIntegral v1 > v2 then trueObj else falseObj
  (FloatObj v1, IntObj v2) -> if v1 > fromIntegral v2 then trueObj else falseObj
  (_, _) -> ErrorObj (TypeError pos ("GreaterThan operator only supports numbers: " ++ objectToString left ++ " > " ++ objectToString right))

isTruthy :: Object -> Bool
isTruthy obj = case obj of
  NullObj -> False
  BoolObj True -> True
  BoolObj False -> False
  IntObj v1 -> v1 /= 0
  _ -> False
