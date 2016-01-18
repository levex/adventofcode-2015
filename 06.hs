{-# LANGUAGE OverloadedStrings #-}

module ProbablyAFireHazard where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.IORef
import qualified Data.Vector.Mutable as VM
import qualified Data.Text as DT

type Map = VM.IOVector Bool
type Pos = (Int, Int)

data Action
  = Action (Bool -> Bool) Pos Pos

createMap :: Int -> Int -> IO Map
createMap w h
  = VM.replicate (w * h) False

applyAction :: Map -> Action -> IO ()
applyAction m (Action f (fx, fy) (tx, ty)) =
  forM_ coords (VM.modify m f)
  where
    coords = [y + x * 1000 | x <- [fx..tx], y <- [fy..ty]]

----------------------------------------------------------
-- Parsing

answer :: String -> IO ()
answer fn = do
  fc <- readFile fn
  m  <- createMap 1000 1000
  forM_ (lines fc) (\line -> do
    let act = fromRight (parseOnly action (DT.pack line))
    m `applyAction` act
    )
  i <- newIORef 0
  forM_ [0..1000 * 1000 - 1] (\j -> do
    b <- VM.read m j
    when b $
      modifyIORef i (+ 1))
  iV <- readIORef i
  print iV

fromRight (Right a) = a
fromRight _         = error "failed to parse"

action :: Parser Action
action = Action <$> parseAction
                <*> (" " *> parsePos <* " through ")
                <*> parsePos

parseAction :: Parser (Bool -> Bool)
parseAction
  =   ("toggle"   >> pure not)
  <|> ("turn on"  >> pure (const True))
  <|> ("turn off" >> pure (const False))

parsePos :: Parser Pos
parsePos
  = (,) <$> (decimal <* char ',') <*> decimal
