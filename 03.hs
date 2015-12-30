module PerfectlySphericalHousesInAVacuum where

import qualified Data.Set as Set
import           System.IO.Unsafe

type Pos = (Int, Int)

countHouses :: Pos -> Set.Set Pos -> String -> Int
countHouses _ s []
  = length s
countHouses (hx, hy) set (s : ss)
  = countHouses np ns ss
  where
    np    = case s of
      '^' -> (hx, hy - 1)
      'v' -> (hx, hy + 1)
      '>' -> (hx + 1, hy)
      '<' -> (hx - 1, hy)
    ns = Set.insert np set

countFile :: String -> Int
countFile
  = countHouses sp (Set.fromList [sp]) . unsafePerformIO . readFile
  where
    sp = (0, 0)
