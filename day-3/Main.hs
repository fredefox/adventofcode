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
