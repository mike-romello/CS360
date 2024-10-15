{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can also load the file into GHCi after starting it by typing `:load
-- HaskellIntro.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.
--
-- Load this file into GHCi and type `isThisWorking` at the prompt. GHCi will
-- tell you whether it's working!
--

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10
--lastDigit = error "lastDigit not yet defined"

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10
--dropLastDigit = error "dropLastDigit not yet defined"

toDigits :: Integer -> [Integer]
toDigits n  | n > 0 = toDigits (dropLastDigit n) ++ [lastDigit n]
            | otherwise = []
--toDigits = error "toDigits not yet defined"

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . double . reverse
  where
    double [] = []
    double (x:[]) = [x]
    double (x:y:zs) = x : (2 * y) : double zs 
--doubleEveryOther = error "doubleEveryOther not yet defined"

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toSingleDigits
  where
    toSingleDigits n
      | n < 10    = [n]
      | otherwise = toSingleDigits (n `div` 10) ++ [n `mod` 10]
--sumDigits = error "sumDigits not yet defined"

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
--validate = error "validate not yet defined"

--
-- Problem 2
--

data Wobbly a = Stable a | Wobbly a
  deriving (Eq, Ord, Show)

onlyStable :: [Wobbly a] -> [a]
onlyStable [] = []
onlyStable ((Stable x):xs) = x : onlyStable xs
onlyStable ((Wobbly _):xs) = onlyStable xs
--onlyStable = error "onlyStable not yet defined"

mapWobbly :: (a -> b) -> [Wobbly a] -> [Wobbly b]
mapWobbly _ [] = []
mapWobbly f ((Stable x):xs) = Stable (f x) : mapWobbly f xs
mapWobbly f ((Wobbly x):xs) = Wobbly (f x) : mapWobbly f xs
--mapWobbly = error "mapWobbly not yet defined"

splitWobbly :: [Wobbly a] -> ([a], [a])
splitWobbly wobblyList =  let (stables, wobblies) = go wobblyList ([], [])
                          in (reverse stables, reverse wobblies)
  where
    go [] acc = acc
    go (Stable x : xs) (stables, wobblies) = go xs (x : stables, wobblies)
    go (Wobbly x : xs) (stables, wobblies) = go xs (stables, x : wobblies)
--splitWobbly = error "splitWobbly not yet defined"

--
-- Problem 3
--

pow :: (a -> a) -> Int -> a -> a
pow f n x
  | n > 0     = f (pow f (n - 1) x)
  | n == 0    = x
  | otherwise = error "Negative exponent not supported"

--pow = error "pow not yet defined"

g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n - 1)
--g = error "g not yet defined"

h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n - 1)
--h = error "h not yet defined"

d :: Int -> Integer -> Integer
d i n
  | n == 0    = 0
  | otherwise = n - pow (d i) i (n - 1)
--d = error "d not yet defined"
