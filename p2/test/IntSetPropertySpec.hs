module IntSetPropertySpec (
    spec
  ) where

import Data.List
import Test.Hspec
import Test.QuickCheck

import IntSet (IntSet(..))
import qualified IntSet as IS

instance Arbitrary IntSet where
  arbitrary = sized (\s -> arbIntSet s 0)
    where
      arbIntSet :: Int -- ^ Size
                -> Int -- ^ Lower bound
                -> Gen IntSet
      arbIntSet 0    _  = return $ IntSet []
      arbIntSet size lb = frequency [(1,    return $ IntSet []),
                                     (size, arbIntSetRec)]
        where
          arbIntSetRec :: Gen IntSet
          arbIntSetRec = do
              NonNegative lo    <- fmap (`mod` 100) <$> arbitrary
              NonNegative delta <- fmap (`mod` 100) <$> arbitrary
              IntSet xs <- arbIntSet (size-1) (lo+delta+lb+2)
              return $ IntSet ((lo+lb,lo+delta+lb):xs)

  shrink (IntSet xs) = map IntSet $ removeOne xs ++ shrinkOne xs
    where
      removeOne :: [a] -> [[a]]
      removeOne []     = []
      removeOne [x]    = [[]]
      removeOne (x:xs) = xs : map (x:) (removeOne xs)

      shrinkOne :: [(Int,Int)] -> [[(Int,Int)]]
      shrinkOne []                       = []
      shrinkOne ((lo,hi):xs) | hi == lo  = shrinkOne xs
                             | otherwise = ((lo+1,hi):xs) :
                                           ((lo,hi-1):xs) :
                                           map ((lo,hi):) (shrinkOne xs)

spec :: Spec
spec = do
  describe "IntSet Property Tests" $ do
    it "valid: Arbitrary IntSet is valid" $
      property prop_valid
    it "empty: model of empty set is empty" $
      property prop_empty
    it "member: x is a member of set iff it is a member of the model" $
      property prop_member
    it "merge: merge of two sets equal to merge of model" $
      property prop_merge
    it "insert: inserting into set equivalent to inserting into model" $
      property prop_insert
    it "delete: deleting from set equivalent to deleting from model" $
      property prop_delete

-- | Model an IntSet by a list of Ints in the set
model :: IntSet -> [Int]
model (IntSet xs) = normalize $ concat [[lo..hi] | (lo,hi) <- xs]

-- | Normalize a list of integers so that it is sorted without duplicates
normalize :: [Int] -> [Int]
normalize = nub . sort

-- | The IntSet invariant from the problem description
invariant :: IntSet -> Bool
invariant (IntSet xs) =
    all (\(lo,hi) -> lo <= hi) xs &&
    all (\((_,hi), (lo,_)) -> lo > hi + 1) (xs `zip` drop 1 xs)

prop_valid :: IntSet -> Bool
prop_valid xs = invariant xs

prop_empty :: Bool
prop_empty = 
  model IS.empty == []
    && invariant IS.empty
--prop_empty = IS.toList IS.empty == []
--prop_empty = model IS.empty == []
--prop_empty = error "prop_empty: undefined"

prop_member :: Int -> IntSet -> Bool
prop_member n s =
  IS.member n s == elem n (model s)
--prop_member x xs = IS.member x xs == (x `elem` model xs)
--prop_member x s = IS.member x s == (x `elem` model s)
--prop_member = error "prop_member: undefined"

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys  -- If the first list is empty, return the second list
merge xs [] = xs  -- If the second list is empty, return the first list
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)  -- If the head of the first list is smaller, include it and recurse
    | x == y = x : merge xs ys  -- To handle duplicates like a set
    | otherwise = y : merge (x:xs) ys  -- Otherwise, include the head of the second list and recurse

prop_merge :: IntSet -> IntSet -> Bool
prop_merge s1 s2 =
  model my_merge == merge (model s1) (model s2) 
    && invariant my_merge
    && IS.merge s1 s2 == IS.merge s2 s1
  where my_merge = IS.merge s1 s2
--prop_merge xs ys = model (IS.merge xs ys) == IS.toList (IS.merge (IS.fromList (model xs)) (IS.fromList (model ys)))
--prop_merge s1 s2 = normalize (model (IS.merge s1 s2)) == normalize (model s1 ++ model s2)
--prop_merge = error "prop_merge: undefined"

prop_insert :: Int -> IntSet -> Bool
--prop_insert x xs = IS.toList (IS.insert x xs) == insert x (model xs) NOT WORKING TEST
prop_insert n s =
  model my_insert == nub (insert n (model s)) 
    && invariant my_insert
  where my_insert = IS.insert n s
--prop_insert x s = 
--  let s' = IS.insert x s
--      l = IS.toList s'
--  in x `elem` l && all (`elem` l) (IS.toList s) && invariant s'
--  let s' = IS.insert x s
--      m = model s'
--  in invariant s' && (x `elem` m) && all (`elem` m) (model s)
--prop_insert = error "prop_insert: undefined"

prop_delete :: Int -> IntSet -> Bool
--prop_delete x xs = IS.toList (IS.delete x xs) == insert x (model xs) NOT WORKING TEST
prop_delete n s = 
  model my_delete == delete n (model s)
    && invariant my_delete
  where my_delete = IS.delete n s
--prop_delete x s =
--  let s' = IS.delete x s
--      m = model s'
--  in invariant s' && not (x `elem` m) && all (\e -> e == x || e `elem` m) (model s)
--prop_delete = error "prop_delete: undefined"
