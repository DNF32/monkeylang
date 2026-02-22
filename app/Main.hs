module Main where

import Data.List (intercalate)
import Eval (Object)
import Eval qualified as MyLib
import MyLib qualified
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  fileName <- getArgs
  contents <- readFile (head fileName)
  let obj = MyLib.interpreter contents
  putStrLn (show obj)
