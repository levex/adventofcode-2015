{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SomeAssemblyRequired where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except
import Data.Monoid

type Wire = String

data Connection
  = Value      Int  Wire
  | NotGate    Wire Wire
  | LShiftGate Wire Int  Wire
  | RShiftGate Wire Int  Wire
  | AndGate    Wire Wire Wire
  | OrGate     Wire Wire Wire

-- Parsing

data ParserError
  = OtherError String
  | EOFError
  | SatisfyFailedError
  | CollectionError [ParserError]

type ParserState = String

newtype Parser a =
    Parser { runParser :: ExceptT ParserError (State ParserState) a }
              deriving (Functor, Monad, Applicative, Alternative,
                        MonadState ParserState, MonadError ParserError)

instance Monoid ParserError where
  mempty = CollectionError []
  mappend (CollectionError e1) e2 = CollectionError (e2 : e1)
  mappend e2 (CollectionError e1) = CollectionError (e2 : e1)

item :: Parser Char
item = do
  s <- get
  case s of
    []       -> throwError EOFError
    (c : cs) -> do
      put cs
      return c

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then
    return c
  else throwError SatisfyFailedError
