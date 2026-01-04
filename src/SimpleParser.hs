{-# LANGUAGE MultiParamTypeClasses #-}

module SimpleParser (SimpleParserError (..), SimpleParser (..)) where

import Control.Applicative (Alternative (..))

class SimpleParserError e s where
  emptyError :: s -> e

newtype SimpleParser s e a
  = SimpleParser {run :: s -> Either e (a, s)}

instance Functor (SimpleParser s e) where
  fmap :: (a -> b) -> (SimpleParser s e) a -> (SimpleParser s e) b
  fmap f sp = SimpleParser $ \s -> case run sp s of
    Right
      ( a,
        newState
        ) -> Right (f a, newState)
    Left errorMsg -> Left errorMsg

instance Applicative (SimpleParser s e) where
  pure :: a -> (SimpleParser s e) a
  pure a = SimpleParser $ \s -> Right (a, s)
  (<*>) :: (SimpleParser s e) (a -> b) -> (SimpleParser s e) a -> (SimpleParser s e) b
  spab <*> spa = SimpleParser $ \state -> case run spab state of
    Right
      ( ab,
        newState
        ) -> run (fmap ab spa) newState
    Left errorMsg -> Left errorMsg

instance Monad (SimpleParser s e) where
  (>>=) :: (SimpleParser s e) a -> (a -> (SimpleParser s e) b) -> (SimpleParser s e) b
  spa >>= aspb = SimpleParser $ \state -> case run spa state of
    Left errorMsg -> Left errorMsg
    Right
      ( a,
        newState
        ) -> run (aspb a) newState

emptySimpleParser :: (SimpleParserError e s) => SimpleParser s e a
emptySimpleParser =
  SimpleParser $ \state ->
    Left (emptyError state)

instance (SimpleParserError e s) => Alternative (SimpleParser s e) where
  empty = emptySimpleParser
  (<|>) :: (SimpleParser s e) a -> (SimpleParser s e) a -> (SimpleParser s e) a
  la <|> la2 = SimpleParser $ \state -> case run la state of
    Left _ -> run la2 state
    Right lexed -> Right lexed
