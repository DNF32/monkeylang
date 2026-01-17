module Main where

import Control.Applicative (Alternative (..))
import Control.Lens
import Test.Hspec
import Test.QuickCheck
import Token

--- This thing should we moved to
mockLexerState :: LexerState
mockLexerState = LexerState {getInput = "\nthis", currentPosition = Position {_line = 1, _column = 1}}

initialState :: String -> LexerState
initialState input = LexerState {getInput = input, currentPosition = Position {_line = 1, _column = 1}}

runAndGetPosition :: Lexer a -> String -> Either LexError Position
runAndGetPosition la input = case runLexer la (initialState input) of
  Right (_, state) -> Right (currentPosition state)
  Left err -> Left err

main :: IO ()
main = putStrLn "that"

spec :: Spec
spec = do
  describe "Position tracking" $ do
    describe "Single character parsing" $ do
      it "starts at line 1, column 1" $ do
        let state = initialState "a"
        currentPosition state `shouldBe` Position 1 1

      it "advances column by 1 when cosuming 1 char" $ do
        let lexerA = charL 'a'
        let result = runLexer lexerA (initialState "a")
        case result of
          Right (_, newState) -> currentPosition newState `shouldBe` Position 1 2
          Left _ -> expectationFailure "Lexer failed"

      it "advances column by 1 for multiple chars" $ do
        let result = runLexer (charL 'a' *> charL 'b' *> charL 'c') (initialState "abc")
        case result of
          Right (_, state) -> currentPosition state `shouldBe` Position 1 4
          Left _ -> expectationFailure "Lexer failed"

    describe "Tab handling" $ do
      it "advances column by 4 for tab" $ do
        let result = runLexer tab (initialState "\t")
        case result of
          Right (_, state) -> currentPosition state `shouldBe` Position 1 5
          Left _ -> expectationFailure "Lexer failed"

      it "handles tabs and spaces mixed" $ do
        let lexer = tab *> ws *> tab
        let result = runLexer lexer (initialState "\t \t")
        case result of
          Right (_, state) -> currentPosition state `shouldBe` Position 1 10 -- 4 + 1 + 4 + 1
          Left _ -> expectationFailure "Lexer failed"

    describe "Carriage return handling" $ do
      it "ignores carriage return (position unchanged)" $ do
        let result = runLexer cr (initialState "\r")
        case result of
          Right (_, state) -> currentPosition state `shouldBe` Position 1 1
          Left _ -> expectationFailure "Lexer failed"

      it "handles CRLF correctly" $ do
        let lexer = cr *> nl
        let result = runLexer lexer (initialState "\r\n")
        case result of
          Right (_, state) -> currentPosition state `shouldBe` Position 2 1
          Left _ -> expectationFailure "Lexer failed"

  describe "Error position tracking" $ do
    it "reports correct position on error" $ do
      let result = runLexer (charL 'a') (initialState "b")
      case result of
        Left err -> (err ^. errorPosition) `shouldBe` Position 1 1
        Right _ -> expectationFailure "Should have failed"

    it "reports correct position on error mid-input" $ do
      let lexer = charL 'a' *> charL 'b' *> charL 'c'
      let result = runLexer lexer (initialState "abx")
      case result of
        Left err -> (err ^. errorPosition) `shouldBe` Position 1 3
        Right _ -> expectationFailure "Should have failed"

    it "reports correct position on error after newline" $ do
      let lexer = charL 'a' *> nl *> charL 'b'
      let result = runLexer lexer (initialState "a\nx")
      case result of
        Left err -> (err ^. errorPosition) `shouldBe` (Position 2 1)
        Right _ -> expectationFailure "Should have failed"
