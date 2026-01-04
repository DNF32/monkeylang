module Main where

import Ast (showExprTree, showTree)
import Control.Applicative (Alternative (..))
import Parser
import Test.Hspec
import Test.QuickCheck
import Token

--- This thing should we moved to
mockLexerState :: LexerState
mockLexerState = LexerState {getInput = "\nthis", currentPosition = Position {line = 1, column = 1}}

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {line = 1, column = 1}}

main :: IO ()
main = undefined

test :: IO ()
test =
  let result = runAstParser (parseExpressionRbp 0) (initialState "10*16 - ;5")
   in case result of
        Right (expr, state) -> putStrLn $ showExprTree expr
        Left err -> putStrLn $ "Error: " ++ show err
