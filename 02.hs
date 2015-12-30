module IWasToldThereWouldBeNoMath where

import Data.List.Split
import System.IO.Unsafe

parse :: String -> [(Int, Int, Int)]
parse
  = f . splitOneOf "x\n"
  where
    f :: [String] -> [(Int, Int, Int)]
    f (l : w : h : xs)
      = (read l, read w, read h) : f xs
    f _
      = []

smallestSide :: Int -> Int -> Int -> Int
smallestSide l w h
  = minimum [l * w, l * h, w * h]

calc :: [(Int, Int, Int)] -> Int
calc []
  = 0
calc ((l, w, h) : xs)
  = 2 * l * w
  + 2 * w * h
  + 2 * h * l
  + smallestSide l w h
  + calc xs

calcFile :: String -> Int
calcFile
  = calc . parse . unsafePerformIO . readFile
