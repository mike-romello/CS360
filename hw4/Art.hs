{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- DO NOT change the module declaration or export list!
module Art (
  Point,
  Exp(..),
  eval,
  build,

  favoriteGray,
  favoriteColor,

  gradient,
  wavy,
  weird,
  trippyR, trippyG, trippyB
) where

import Data.Data ( Data, Typeable )

import ImageUtils ( RandomDoubles, splitRandomDoubles )

import Prelude hiding (sin, cos)
import qualified Prelude (sin, cos)
-- You can start GHCi at the UNIX prompt with the command `stack ghci
-- --ghci-options -fobject-code`.
--
-- You can also load the file into GHCi after starting it by typing `:load
-- Art.hs` once GHCi has started.
--
-- You can reload a file in GHCi after making changes by typing `:reload`.
--

--
-- Problem 1: Expression evaluation
--

data Exp = X           -- Value of x
         | Y           -- Value of y
         | Sin Exp     -- sin (pi * e)
         | Cos Exp     -- cos (pi * e)
         | Mul Exp Exp -- Product of e1 and e2
         | Avg Exp Exp -- Average of e1 and e2
         | Blend Exp Exp Exp  -- New: Weighted blend of e2 and e3, weighted by e1
         | Oscillate Exp Exp Exp -- New: Oscillates between e2 and e3 based on e1
         | Pulse Exp Exp Exp -- New: Modulates e1 based on the sine of e2 and e3
-- DO NOT change the deriving clause!
  deriving (Eq, Ord, Show, Data, Typeable)

type Point = (Double, Double)

sin :: Double -> Double
sin = Prelude.sin . (pi *)

cos :: Double -> Double
cos = Prelude.cos . (pi *)

-- The eval function evaluates an expression at a given point
eval :: Exp -> Point -> Double
eval X (x, _) = x
eval Y (_, y) = y
eval (Sin exp) point = sin (eval exp point)
eval (Cos exp) point = cos (eval exp point)
eval (Mul e1 e2) point = (eval e1 point) * (eval e2 point)
eval (Avg e1 e2) point = (eval e1 point + eval e2 point) / 2
eval (Blend e1 e2 e3) point =
  let w = eval e1 point * 0.5 + 0.5 -- normalize to [0,1]
  in (1 - w) * eval e2 point + w * eval e3 point
eval (Oscillate e1 e2 e3) point =
  if sin (eval e1 point) > 0 then eval e2 point else eval e3 point
eval (Pulse e1 e2 e3) point =
  (sin (eval e2 point + eval e3 point) * 0.5 + 0.5) * eval e1 point


--
-- Problem 2: Building random expressions
--

build :: Int -> RandomDoubles -> Exp
build _ [] = error "build: the impossible happened" -- Handling the empty list case
build depth (r:rs)
  | depth <= 0 = if r < 0.5 then X else Y -- Simple base case
  | r < 0.01 = X -- Decrease probability for X
  | r < 0.02 = Y -- Decrease probability for Y
  | r < 0.4 = Sin (build (depth - 1) rs) -- Increase depth usage, use 'rs' directly
  | r < 0.6 = Cos (build (depth - 1) rs) -- Increase depth usage, use 'rs' directly
  | r < 0.75 = Mul (build (depth - 1) rs) (build (depth - 1) rs) -- Adjust for balanced expression creation, use 'rs' directly
  | r < 0.85 = Blend (build (depth - 1) rs) (build (depth - 1) rs) (build (depth - 1) rs)
  | r < 0.9 = Oscillate (build (depth - 1) rs) (build (depth - 1) rs) (build (depth - 1) rs)
  | otherwise = Pulse (build (depth - 1) rs) (build (depth - 1) rs) (build (depth - 1) rs)

--
-- Problem 3: Submit Your Favorite Image
--
-- Provide (seed, depth) pairs for your favorite grayscale and color images.
--

favoriteGray :: (Int, Int)
favoriteGray = (5, 10)

favoriteColor :: (Int, Int)
favoriteColor = (9, 6)

--
-- Sample expressions
--

gradient :: Exp
gradient = Avg X Y

wavy :: Exp
wavy = Avg (Cos X) (Sin Y)

weird :: Exp
weird = Mul (Sin (Sin (Sin (Sin (Sin (Sin (Sin (Cos Y))))))))
            (Cos (Sin (Cos (Avg (Sin Y) (Mul X X)))))

trippyR, trippyG, trippyB :: Exp
trippyR = Sin (Avg (Mul (Mul (Mul (Cos (Mul (Sin (Cos Y)) (Avg (Avg X X) (Sin Y)))) (Avg (Sin (Mul (Sin Y) (Mul Y X))) (Cos (Cos (Mul Y Y))))) (Sin (Mul (Sin (Mul (Sin Y) (Sin Y))) (Cos (Mul (Mul Y Y) (Sin Y)))))) (Sin (Avg (Cos (Avg (Mul (Mul Y X) (Mul X X)) (Sin (Mul Y X)))) (Sin (Avg (Avg (Sin X) (Avg X X)) (Sin (Avg X Y))))))) (Cos (Cos (Avg (Sin (Sin (Avg (Mul X X) (Mul X X)))) (Sin (Sin (Sin (Sin Y))))))))

trippyG = Sin (Mul (Mul (Avg (Avg (Cos (Mul (Cos (Cos X)) (Mul (Cos X) (Avg Y X)))) (Mul (Mul (Cos (Cos Y)) (Mul (Cos X) (Mul X Y))) (Sin (Sin (Avg Y Y))))) (Cos (Mul (Avg (Sin (Sin X)) (Sin (Sin X))) (Sin (Sin (Mul X Y)))))) (Avg (Mul (Avg (Cos (Sin (Cos X))) (Avg (Mul (Sin X) (Cos Y)) (Avg (Cos X) (Cos X)))) (Avg (Avg (Sin (Cos X)) (Sin (Sin X))) (Mul (Avg (Cos X) (Avg Y X)) (Avg (Sin Y) (Sin X))))) (Mul (Cos (Cos (Mul (Avg Y Y) (Mul Y X)))) (Cos (Cos (Sin (Avg X X))))))) (Sin (Avg (Avg (Sin (Cos (Sin (Cos X)))) (Avg (Sin (Cos (Cos Y))) (Mul (Mul (Sin Y) (Mul X Y)) (Cos (Mul Y Y))))) (Cos (Avg (Mul (Mul (Cos Y) (Mul Y Y)) (Avg (Sin Y) (Cos Y))) (Mul (Mul (Mul X X) (Avg Y X)) (Cos (Sin X))))))))

trippyB = Avg (Sin (Mul (Avg (Cos (Avg (Mul (Cos (Mul X X)) (Cos (Mul X Y))) (Avg (Avg (Mul X X) (Avg Y Y)) (Avg (Cos Y) (Cos X))))) (Avg (Avg (Avg (Mul (Sin Y) (Mul X Y)) (Sin (Mul X X))) (Avg (Mul (Mul X X) (Sin Y)) (Mul (Avg X X) (Sin Y)))) (Avg (Mul (Cos (Sin Y)) (Cos (Avg X X))) (Sin (Avg (Sin Y) (Sin Y)))))) (Cos (Avg (Avg (Avg (Sin (Mul X X)) (Avg (Sin Y) (Sin X))) (Cos (Avg (Cos Y) (Avg Y X)))) (Mul (Mul (Mul (Avg X Y) (Cos X)) (Cos (Avg Y X))) (Avg (Cos (Mul Y X)) (Mul (Mul X X) (Mul Y X)))))))) (Avg (Mul (Mul (Mul (Mul (Sin (Sin (Avg X X))) (Avg (Avg (Sin Y) (Sin Y)) (Avg (Avg X X) (Cos Y)))) (Sin (Sin (Sin (Mul Y Y))))) (Avg (Cos (Avg (Avg (Avg X Y) (Mul Y X)) (Cos (Sin X)))) (Mul (Sin (Sin (Sin X))) (Cos (Mul (Mul Y Y) (Cos X)))))) (Avg (Cos (Cos (Sin (Cos (Avg X Y))))) (Mul (Sin (Mul (Cos (Avg Y X)) (Sin (Cos X)))) (Mul (Mul (Sin (Cos Y)) (Avg (Avg X X) (Cos X))) (Avg (Mul (Sin X) (Avg Y X)) (Sin (Sin X))))))) (Mul (Mul (Cos (Cos (Mul (Sin (Mul Y Y)) (Cos (Cos X))))) (Avg (Sin (Avg (Cos (Sin Y)) (Mul (Cos X) (Avg X X)))) (Cos (Cos (Cos (Avg X Y)))))) (Sin (Mul (Avg (Mul (Cos (Mul Y Y)) (Cos (Sin Y))) (Avg (Mul (Mul X X) (Sin X)) (Cos (Sin Y)))) (Avg (Sin (Mul (Avg Y X) (Avg X X))) (Cos (Avg (Mul Y Y) (Avg Y Y))))))))
