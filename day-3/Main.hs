radius :: Int -> Int
radius n = (sqrt' n + 1) `div` 2
  where
    sqrt' = floor . sqrt . fromIntegral

solve :: Int -> Int
solve n = r + distToMedian r n
  where
    r      = radius n

distToMedian :: Int -> Int -> Int
distToMedian r n = minimum diffs
  where
    diffs  = map (diff n) (lins r)
    diff n m = abs (n - m)

lins r = [f k | k <- [1,3,5,7]]
  where
    f k = 4 * (r^2 + r) - k * r

main = print . solve . rephrase $ input

rephrase = pred

input = 325489

test = take 3 $ part2 ([1, 2, 4, 5, 10, 11, 13, 25], 2)

part2 :: ([Int], Int) -> [[Int]]
part2 (xs, n) = rest:(part2 (rest, n+1))
  where
    lxs = length xs
    (side, dist) = factor lxs n
    rest :: [Int]
    rest = scanl step (last xs) [0..8*n-2]
    step :: Int -> Int -> Int
    step prev ix = prev + neighborsFromBefore ((ix + n') `mod` (8 * n))
    n' = n - 1
    neighborsFromBefore ix
      = xs !! (side * n') + dist
-- one-off might occur here:
--      + xs !! (side * n') + dist - 1

factor :: Int -> Int -> (Int, Int)
factor n m = (n `div` m, n `mod` m)
