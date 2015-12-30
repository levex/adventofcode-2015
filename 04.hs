module TheIdealStockingBuffer where

import Data.Hash.MD5
import Data.List

findAnswer :: String -> Int
findAnswer str
  = findAnswer' str 0
  where
    findAnswer' :: String -> Int -> Int
    findAnswer' str n
      | "00000" `isPrefixOf` md5 = n
      | otherwise                = findAnswer' str (n + 1)
      where
        md5 = md5s . Str $ str ++ show n
