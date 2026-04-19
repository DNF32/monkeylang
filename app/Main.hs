module Main where

import Ast
import Data.List (intercalate)
import Eval (Object (..), interpreter, toSourceCode)
import Parser
import System.Environment (getArgs)
import System.IO
import Token
import TypeChecker
import Types

main :: IO ()
main = do
  args <- getArgs
  case (head args) of
    "--check" -> do
      contents <- readFile (args !! 1)
      case typeChecker contents of
        Left err -> putStrLn (show err)
        Right (_, env) -> putStrLn "No error "
    _ -> do
      contents <- readFile (head args)
      let obj = interpreter contents
      let sourceCode = case obj of
            ErrorObj err ->
              let src = toSourceCode (contents) err
               in (src ++ "\n" ++ show obj)
            _ -> (show obj)
      putStrLn (sourceCode)
