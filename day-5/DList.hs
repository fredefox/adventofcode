module DList
  ( DList()
  , fromList
  , forward
  , backward
  , stepForward
  , stepBackward
  , step
  , current
  , modifyCurrent
  , Pretty(..)
  ) where

import Data.Monoid
import Control.Monad ((>=>))

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty a => Pretty (DList a) where
  pretty (DList xs ys) = s (reverse xs) ++ '|':(s ys)
    where
      s = concatMap pretty

data DList a = DList [a] [a] deriving (Eq, Show)

instance Monoid (DList a) where
  mempty = DList mempty mempty
  (DList l0 r0) `mappend` (DList l1 r1) = DList l0 (r0 <> reverse l1 <> r1)

fromList :: [a] -> DList a
fromList = DList mempty

forward :: DList a -> Maybe (DList a)
forward (DList l r) = case r of
  [] -> Nothing
  x:xs -> Just $ DList (x:l) xs

backward :: DList a -> Maybe (DList a)
backward (DList l r) = case l of
  [] -> Nothing
  x:xs -> Just $ DList xs (x:r)

step, stepForward, stepBackward :: Integer -> DList a -> Maybe (DList a)
step n
  | n < 0 = stepBackward (-n)
  | n > 0 = stepForward n
  | n == 0 = return

stepForward n = repeatM n forward
stepBackward n = repeatM n backward

current :: DList a -> Maybe a
current (DList _ xs) = case xs of
  [] -> Nothing
  x:_ -> Just x

modifyCurrent :: (a -> a) -> DList a -> DList a
modifyCurrent f dl@(DList l r) = case r of
  [] -> dl
  x:xs -> DList l ((f x):xs)

repeatM :: Monad m => Integer -> (a -> m a) -> a -> m a
repeatM 0 _ = return
repeatM n act = act >=> repeatM (n-1) act
