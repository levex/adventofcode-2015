{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SomeAssemblyRequired where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import           Data.Monoid
import qualified Data.Map.Strict as M

type Wire = String

data Connection
  = Value      Int  Wire
  | NotGate    Wire Wire
  | LShiftGate Wire Int  Wire
  | RShiftGate Wire Int  Wire
  | AndGate    Wire Wire Wire
  | OrGate     Wire Wire Wire
  | ErrorConnection
    deriving (Show)

type MachineState = M.Map String Int

executeConnection :: MachineState -> Connection -> MachineState
executeConnection = undefined

-- λ
-- Parsing

data ParserError
  = OtherError String
  | EOFError
  | SatisfyFailedError
  | CollectionError [ParserError]
    deriving (Show)

type ParserState = String

newtype Parser a =
    Parser { runParser :: ExceptT ParserError (State ParserState) a }
              deriving (Functor, Monad, Applicative, Alternative,
                        MonadState ParserState, MonadError ParserError)

runMonadic
  = runState . runExceptT . runParser

parse :: Parser a -> String -> Either ParserError a
parse
  = (.) fst . runMonadic

instance Monoid ParserError where
  mempty = CollectionError []
  mappend (CollectionError e1) e2 = CollectionError (e2 : e1)
  mappend e2 (CollectionError e1) = CollectionError (e2 : e1)
  mappend e1 e2                   = CollectionError [e1, e2]

item :: Parser Char
item = do
  s <- get
  case s of
    []       -> throwError EOFError
    (c : cs) -> do
      put cs
      return c

try :: Parser a -> Parser a
try p = do
  init <- get
  p `catchError` \e -> do
    put init
    throwError e

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = try $ do
  c <- item
  if p c then
    return c
  else throwError SatisfyFailedError

char :: Char -> Parser Char
char = satisfy . (==)

(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>) = liftA2 (:)

string :: String -> Parser String
string str = do
  case str of
    [] -> return []
    (c : cs) -> char c <:> string cs

whitespace :: Parser String
whitespace = many $ char ' '

wsstring :: String -> Parser String
wsstring str = whitespace *> string str

digit :: Parser Char
digit = satisfy (liftA2 (&&) (>= '0') (<= '9'))

number :: Parser Int
number = read <$> some digit

alphabetic :: Parser Char
alphabetic = satisfy (`elem` ['a'..'z'])

value :: Parser Int
value = whitespace *> number

arrow :: Parser String
arrow = whitespace *> string "->"

wire :: Parser Wire
wire = whitespace *> some alphabetic

line :: Parser Connection
line =   try (Value      <$> value <*  arrow <*> wire)
     <|> try ((string "NOT") *> (NotGate    <$> wire  <*  arrow <*> wire))
     <|> try ((LShiftGate <$> (try (wire <|> value) <* (wsstring "LSHIFT")) <*> value <* arrow <*> wire))
     <|> try ((RShiftGate <$> (try (wire <|> value) <* (wsstring "RSHIFT")) <*> value <* arrow <*> wire))
     <|> try ((AndGate    <$> (try (wire <|> value) <* (wsstring "AND"))    <*> wire  <* arrow <*> wire))
     <|> try ((OrGate     <$> (try (wire <|> value) <* (wsstring "OR"))     <*> wire  <* arrow <*> wire))
     <|> (throwError $ OtherError "Invalid input")

parseFile :: String -> IO [Connection]
parseFile file = do
  c <- lines <$> readFile file
  mapM (\e -> (f (parse line e))) c
  where
    f (Left _) = return ErrorConnection
    f (Right s) = return s
