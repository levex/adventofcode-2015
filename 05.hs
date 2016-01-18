module DoesntHeHaveInternElvesForThis where

import Data.List
import System.IO.Unsafe

isNice :: String -> Bool
isNice str
  =  length (filter (`elem` "aeiou") str) >= 3
  && checkDouble ' ' str
  && not ("ab" `isInfixOf` str)
  && not ("cd" `isInfixOf` str)
  && not ("pq" `isInfixOf` str)
  && not ("xy" `isInfixOf` str)

countNiceList :: [String] -> Int
countNiceList
  = length . filter isNice

countNiceFile :: String -> Int
countNiceFile
  = countNiceList . lines . unsafePerformIO . readFile

checkDouble :: Char -> String -> Bool
checkDouble _ []
  = False
checkDouble c (s : ss)
  | c == s    = True
  | otherwise = checkDouble s ss
