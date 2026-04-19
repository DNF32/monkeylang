module Main where

import Data.List (intercalate)
import Eval (Object, toSourceCode)
import Eval qualified as MyLib
import MyLib qualified
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  fileName <- getArgs
  contents <- readFile (head fileName)
  let obj = MyLib.interpreter contents

  sourceCode <- case obj of
    MyLib.ErrorObj err -> do
      src <- toSourceCode (head fileName) err
      pure (src ++ "\n" ++ show obj)
    _ -> pure (show obj)
  putStrLn (sourceCode)
