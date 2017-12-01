{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char

inputF :: FilePath
inputF = "/home/fredefox/workspace/adventofcode/day-1/input.data"

solve  :: [Int] -> Int
solve = solve' . wrapAround

solve' :: [Int] -> Int
solve' [] = 0
solve' (x:xs) = fst . foldl go (0, x) $ xs
  where
    go (s, pred) x =
      if pred == x
      then (s + x, x)
      else (s    , x)

wrapAround :: [a] -> [a]
wrapAround [] = []
wrapAround xs@(x:_) = xs ++ [x]

main :: IO ()
main = readFile inputF >>= print . solve . map digitToInt
