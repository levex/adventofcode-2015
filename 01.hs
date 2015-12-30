module NotQuiteLisp where

parse :: String -> Int
parse []
  = 0
parse (x : xs)
  | x == '(' = 1 + parse xs
  | x == ')' = -1 + parse xs

test :: Bool
test
  =  parse "(())"    ==  0
  && parse "()()"    ==  0
  && parse "((("     ==  3
  && parse "(()(()(" ==  3
  && parse "))(((((" ==  3
  && parse "())"     == -1
  && parse "))("     == -1
  && parse ")))"     == -3
  && parse ")())())" == -3
