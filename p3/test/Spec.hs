module Main (
    main,
    spec
  ) where

import Data.List (nub, sort)
import Data.Maybe (fromJust)
import Test.Hspec (hspec, describe, it, shouldBe, Spec, Expectation)

import While.Big (evalS)
import While.Small (stepsS)
import While.Env (Env(..), emptyEnv)
import While.Monad (runWhile)
import While.ParMonad (runParWhile)
import While.Parser (parseWhile)
import While.Syntax (Z, Stm, Var)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "variable" $
    it "attempt to get value of undefined variable" $
      runBig "a := b" `shouldFailWith` "Unbound variable \"b\""
  describe "skip" $
    it "skips (doesn't do anything)" $
      runBig "skip" `shouldBeEnv` []
  describe "assignment" $
    it "assigns 'a' the value 3" $
      runBig "a := 3" `shouldBeEnv` [("a", 3)]
  describe "arithmetic tests" $ do
    it "constant"       $ runBig "x := 3" `shouldBeEnv` [("x", 3)]
    it "addition"       $ runBig "x := 2 + 3" `shouldBeEnv` [("x", 5)]
    it "subtraction"    $ runBig "x := 2 - 3" `shouldBeEnv` [("x", -1)]
    it "multiplication" $ runBig "x := 2 * 3" `shouldBeEnv` [("x", 6)]
    it "division"       $ runBig "x := 6 / 2" `shouldBeEnv` [("x", 3)]
    it "division by 0"  $ runBig "x := 6 / 0" `shouldFailWith` "Division by zero"
  describe "Boolean tests" $ do
    it "true" $
      runBig "if true then x := 1 else x:= 0" `shouldBeEnv` [("x", 1)]
    it "false" $
      runBig "if false then x := 1 else x:= 0" `shouldBeEnv` [("x", 0)]
    it "equals" $
      runBig "if 3 = 3 then x := 1 else x:= 0" `shouldBeEnv` [("x", 1)]
    it "not equal" $
      runBig "if 3 = 4 then x := 1 else x:= 0" `shouldBeEnv` [("x", 0)]
    it "<=" $
      runBig "if 3 <= 10 then x := 1 else x:= 0" `shouldBeEnv` [("x", 1)]
    it "not <=" $
      runBig "if 3 <= 0 then x := 1 else x:= 0" `shouldBeEnv` [("x", 0)]
    it "not" $
      runBig "if ~(3 = 3) then x := 1 else x:= 0" `shouldBeEnv` [("x", 0)]
    it "and" $
      runBig "if 3 = 3 && 4 = 4 then x := 1 else x:= 0" `shouldBeEnv` [("x", 1)]
  describe "Big-step evaluator" $ do
    describe "assignment" $
      it "assigns 'a' the value 1 then the value 2" $
        runBig "a := 1; a := 2" `shouldBeEnv` [("a", 2)]
    describe "sequencing" $
      it "assigns 'a' the value 3 and 'b' the value 4" $
        runBig "a := 3; b := 4" `shouldBeEnv` [("a", 3), ("b", 4)]
    describe "if-then-else" $
      it "assigns 'a' the value 3 and then tests it" $
        runBig "a := 3; if a = 3 then b := 5 else b := 6" `shouldBeEnv` [("a", 3), ("b", 5)]
    describe "while" $
      it "computes the sum of 1 to 10" $
        runBig "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1)" `shouldBeEnv` [("i", 11), ("sum", 55)]
    describe "or" $ do
      it "set x to both 1 and 2" $
        runBig "x := 1 or x := 2" `shouldBeEnv` [("x", 1)]
      it "set x to both 1 and 2 in parallel" $
        runBigPar "x := 1 or x := 2" `shouldBeEnvs` [[("x", 2)], [("x", 1)]]
  describe "Small-step evaluator" $ do
    describe "assignment" $
      it "assigns 'a' the value 1 then the value 2" $
        runSmall "a := 1; a := 2" `shouldBeEnv` [("a", 2)]
    describe "sequencing" $
      it "assigns 'a' the value 3 and 'b' the value 4" $
        runSmall "a := 3; b := 4" `shouldBeEnv` [("a", 3), ("b", 4)]
    describe "if-then-else" $
      it "assigns 'a' the value 3 and then tests it" $
        runSmall "a := 3; if a = 3 then b := 5 else b := 6" `shouldBeEnv` [("a", 3), ("b", 5)]
    describe "while" $
      it "computes the sum of 1 to 10" $
        runSmall "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1)" `shouldBeEnv` [("i", 11), ("sum", 55)]
    describe "par" $ do
      it "set x to both 1 and 2" $
        runSmallPar "x := 1 par x := 2" `shouldBeEnvs` [[("x", 2)], [("x", 1)]]
      it "interleaving" $
        runSmallPar "count := 1; (x := count; count := x+1) par (y := count; count := y+1)"
        `shouldBeEnvs`
        [ [("count", 2), ("x", 1), ("y", 1)]
        , [("count", 3), ("x", 1), ("y", 2)]
        , [("count", 3), ("x", 2), ("y", 1)]
        ]

-- | Check that evaluation resulted in an error
shouldFailWith :: Either String Env -> String -> Expectation
shouldFailWith result msg = result `shouldBe` Left msg

-- | Compare evaluation results, normalizing environments so bindings are
-- sorted.
shouldBeEnv :: Either String Env -> [(Var, Z)] -> Expectation
shouldBeEnv env env' = shouldBe (fmap normalize env) (Right (normalize (Env env')))

-- | Compare non-deterministic evaluation results, normalizing environments so bindings are
-- sorted.
shouldBeEnvs :: [Either String Env] -> [[(Var, Z)]] -> Expectation
shouldBeEnvs envs envs' = shouldBe (nub (sort (map (fmap normalize) envs)))
                                   (nub (sort (map (Right . normalize . Env) envs')))

-- | Normalize the bindings in an environment
normalize :: Env -> Env
normalize (Env vs) = Env (nub (sort vs))

-- | Run the big-step semantics
runBig :: String -> Either String Env
runBig s = fmap snd $ runWhile (evalS (parse s)) emptyEnv
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile

-- | Run the non-deterministic big-step semantics
runBigPar :: String -> [Either String Env]
runBigPar s = map (fmap snd) (runParWhile (evalS (parse s)) emptyEnv)
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile

-- | Run the small-step semantics
runSmall :: String -> Either String Env
runSmall s = fmap snd $ runWhile (stepsS (parse s)) emptyEnv
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile

-- | Run the non-deterministic small-step semantics
runSmallPar :: String -> [Either String Env]
runSmallPar s = map (fmap snd) (runParWhile (stepsS (parse s)) emptyEnv)
  where
    parse :: String -> Stm
    parse = fromJust . parseWhile
