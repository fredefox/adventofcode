import qualified Data.List as L
import DList (DList(), current, modifyCurrent, pretty)
import qualified DList as DL
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

tracePretty x = trace (pretty x)
tracePrettyId x = tracePretty x x

main :: IO ()
main = getContents >>= print . solve . parse

parse :: String -> DList Int
parse = DL.fromList . map read . lines

solve :: DList Int -> Int
solve = either id err . runSolver solver
  where
    err = error "should never happen"

runSolver :: Solver a -> DList Int -> Either Int (Int, DList Int)
runSolver s = runExcept . execStateT s . initEnv

solver :: Solver ()
solver = forever evalStep

evalStep :: Solver ()
evalStep = do
  -- record one step
  -- read and increment current pointer
  n <- readCurrent
  -- get list; step n times in that list; update list-state
  getList >>= stepM n >>= putList

stepM :: Int -> DList a -> Solver (DList a)
stepM n l = liftMaybeCurr (DL.step (fromIntegral n) l)

-- Records that a step has been taken
incr :: Solver ()
incr = modify incr'
  where
    incr' (n, x) = (succ n, x)

rule :: Int -> Int
-- The rule for pt. 1 is just to increment.
-- rule = succ
-- The rule for pt. 2 is like so:
rule n
  | n < 3 = succ n
  | otherwise = pred n

getList :: Solver (DList Int)
getList = gets snd

putList :: DList Int -> Solver ()
putList l = modify putList'
  where
    putList' (n, _) = (n, l)

modList :: (DList Int -> DList Int) -> Solver ()
modList f = modify putList'
  where
    putList' (n, l) = (n, f l)

getCurrent :: Solver Int
getCurrent = getList >>= liftMaybeCurr . current

-- If an 'error' occurs, just exit with the current amount of steps.
liftMaybeCurr :: Maybe a -> Solver a
liftMaybeCurr m = do
  n <- gets fst
  liftMaybe n m

liftMaybe :: Int -> Maybe a -> Solver a
liftMaybe alt Nothing = throwError alt
liftMaybe _ (Just a) = return a

modCurrent :: (Int -> Int) -> Solver ()
modCurrent f = do
  l <- getList
  putList $ modifyCurrent f l

readCurrent :: Solver Int
readCurrent = do
  -- n <- (\x -> trace ("Moving " ++ (show x)) x) <$> getCurrent
  n <- getCurrent
  modCurrent rule
  incr
  return n

-- terminates with the amount of steps on out-of-bounds
type Solver a = StateT (Int, DList Int) (Except Int) a

initEnv :: DList Int -> (Int, DList Int)
initEnv = (,) 0
