import Data.Char
import Data.Maybe

solve :: [[Int]] -> Int
solve = sum . map (diff . extractMaxMin)
  where
    diff :: Maybe (Int, Int) -> Int
    diff = fromMaybe 0 . fmap (uncurry (flip (-)))

extractMaxMin :: [Int] -> Maybe (Int, Int)
extractMaxMin []       = Nothing
extractMaxMin xs@(x:_) = Just $ foldl step (x, x) xs
  where
    step :: (Int, Int) -> Int -> (Int, Int)
    step (mn, mx) m
      | m < mn    = (m, mx)
      | m > mx    = (mn, m)
      | otherwise = (mn, mx)

main :: IO ()
main = getContents >>= print . solve . parse
  where
    parse :: String -> [[Int]]
    parse = map (map read . words) . lines
