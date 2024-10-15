{-# OPTIONS_GHC -fwarn-tabs #-}

module IntSet (
    IntSet(..),

    empty,
    member,
    insert,
    merge,
    delete,

    fromList,
    toList
  ) where

import Data.List (foldl', nub, sort)

newtype IntSet = IntSet [(Int, Int)]
  deriving (Eq, Ord, Show)

-- | Create an empty set
empty :: IntSet
empty = IntSet []
--empty = error "empty undefined"

-- | Check if an integer is in the set
member :: Int -> IntSet -> Bool
member x (IntSet intervals) = any inInterval intervals
  where
    inInterval (low, high) = x >= low && x <= high
--member _ _ = error "member undefined"

-- | Insert an element into the set
insert :: Int -> IntSet -> IntSet
insert x (IntSet []) = IntSet [(x, x)] -- Insert into an empty set
insert x (IntSet s) = IntSet (insert' x s)
  where
    insert' :: Int -> [(Int, Int)] -> [(Int, Int)]
    insert' x [] = [(x, x)] -- If we reach the end, insert the new interval
    insert' x ((lo, hi):rest)
      | x < lo - 1 = (x, x):(lo, hi):rest -- New interval before the first interval
      | x >= lo && x <= hi = (lo, hi):rest -- Already within an interval, no changes
      | x == hi + 1 = (lo, x):rest -- Extend the high bound of the current interval
      | x == lo - 1 = (x, hi):rest -- Extend the low bound of the current interval
      | null rest = [(lo, hi), (x, x)] -- If rest is empty, add a new interval at the end
      | otherwise = let (nextLo, nextHi) = head rest in 
                      if x < nextLo - 1 then 
                        (lo, hi):(x, x):rest -- x is not adjacent to the next interval, insert new
                      else if hi + 1 == nextLo then 
                        (lo, nextHi):tail rest -- Merge with the next interval if adjacent
                      else 
                        (lo, hi):insert' x rest -- Continue to next intervals
--insert _ _ = error "insert undefined"

-- | Union two sets
merge :: IntSet -> IntSet -> IntSet
merge (IntSet xs) (IntSet ys) = IntSet $ mergeIntervals (sort (xs ++ ys))

mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals [] = []
mergeIntervals [x] = [x]
mergeIntervals ((x1, y1):(x2, y2):xs)
  | y1 + 1 >= x2 = mergeIntervals ((x1, max y1 y2):xs) -- Overlapping or adjacent intervals are merged
  | otherwise = (x1, y1) : mergeIntervals ((x2, y2):xs) -- Non-overlapping intervals are kept separate
--merge _ _ = error "merge undefined"

-- | Delete an element from the set
delete :: Int -> IntSet -> IntSet
delete x (IntSet []) = IntSet [] -- Deleting from an empty set
delete x (IntSet s) = IntSet (delete' x s)
  where
    delete' :: Int -> [(Int, Int)] -> [(Int, Int)]
    delete' _ [] = [] -- Base case for recursion
    delete' x ((lo, hi):rest)
      | x < lo = (lo, hi):rest -- x is less than the interval, no deletion
      | x > hi = (lo, hi):delete' x rest -- x is greater than the interval, proceed to next
      | x == lo && x == hi = rest -- x is the only element in the interval, remove it
      | x == lo = (lo + 1, hi):rest -- x is the lower bound, trim the interval
      | x == hi = (lo, hi - 1):rest -- x is the upper bound, trim the interval
      | otherwise = (lo, x - 1):(x + 1, hi):rest -- x is in the middle, split the interval
--delete _ _ = error "delete undefined"

-- | Convert a list of @Int@s to an @IntSet@
fromList :: [Int] -> IntSet
fromList is = foldl' (flip insert) empty is

-- | Convert an @IntSet@ to a list of @Int@s
toList :: IntSet -> [Int]
toList (IntSet xs) = nub $ sort $ concat [[lo..hi] | (lo,hi) <- xs]
