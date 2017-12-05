import Data.List

allUnique :: (Ord a, Eq a) => [a] -> Bool
allUnique = all ((==) 1 . length) . sg

sg :: Ord a => [a] -> [[a]]
sg = group . sort

solvePart1 :: [[String]] -> Int
solvePart1 = length . filter allUnique

part1 :: IO ()
part1 = getContents >>= print . solvePart1 . parse

parse :: String -> [[String]]
parse = map words . lines

allUniqueAng :: (Ord a, Eq a) => [[a]] -> Bool
allUniqueAng = all ((==) 1 . length) . sgAng

-- a `agOf` b iff sort a == sort b
sgAng :: Ord a => [[a]] -> [[[a]]]
sgAng = group . sort . map sort

solvePart2 :: [[String]] -> Int
solvePart2 = length . filter allUniqueAng

part2 :: IO ()
part2 = getContents >>= print . solvePart2 . parse

main :: IO ()
main = part2
