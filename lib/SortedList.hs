module SortedList where

import Data.List (sort)

data SortedList a where
  SortedList :: (Ord a, Show a) => [a] -> SortedList [a]

deriving stock instance Show (SortedList a)

bSearch :: (Ord a) => [a] -> a -> Bool
bSearch [] _ = False
bSearch as a = case compare a middle of
  LT -> bSearch (Prelude.take midpoint as) a
  GT -> bSearch (Prelude.drop (midpoint + 1) as) a
  EQ -> True
  where
    middle = as !! midpoint
    midpoint = floor ((fromIntegral (length as) / 2) :: Double)

bSearchBy :: (Ord a, Ord b) => [a] -> (a -> b) -> b -> Bool
bSearchBy [] _ _ = False
bSearchBy as f b = case compare b middle of
  LT -> bSearchBy (Prelude.take midpoint as) f b
  GT -> bSearchBy (Prelude.drop (midpoint + 1) as) f b
  EQ -> True
  where
    middle = f $ as !! midpoint
    midpoint = floor ((fromIntegral (length as) / 2) :: Double)

take :: SortedList [a] -> Int -> SortedList [a]
take (SortedList as) n = SortedList $ Prelude.take n as

fromList :: (Ord a, Show a) => [a] -> SortedList [a]
fromList as = SortedList $ sort as

insert :: (Ord a, Show a) => SortedList [a] -> a -> SortedList [a]
insert (SortedList []) a = SortedList [a]
insert (SortedList [h]) a = if a <= h then SortedList [a, h] else SortedList [h, a]
insert (SortedList (h : as)) a = if a <= h then SortedList $ a : h : as else SortedList $ h : insert' as a

insert' :: (Ord a, Show a) => [a] -> a -> [a]
insert' [] a = [a]
insert' [h] a = if a <= h then [a, h] else [h, a]
insert' (h : as) a = if a <= h then a : h : as else h : insert' as a
