{-# OPTIONS_GHC -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

module While.Big (
    evalA,
    evalB,
    evalS,
    runBig,
    runBigPar
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad.State (MonadState)

import While.Monad (runWhile)
import While.ParMonad (runParWhile)

-- This is the module where the environment and state are defined
import While.Env (Env, emptyEnv, lookupVar, setVar)

-- This is the module where our abstract syntax is defined
import While.Syntax (Stm(..), Bexp(..), Aexp(..), Z)

-- This is the module that parsers concrete syntax
import While.Parser (parseWhile)

-- | The evaluator for arithmetic expressions
evalA :: (MonadFail m, MonadState Env m) => Aexp -> m Z
evalA (Const z)   = return z
evalA (Var v)     = lookupVar v
evalA (Add a1 a2) = do x <- evalA a1
                       y <- evalA a2
                       return $ x + y
evalA (Sub a1 a2) = do x <- evalA a1
                       y <- evalA a2
                       return $ x - y
evalA (Mul a1 a2) = do x <- evalA a1
                       y <- evalA a2
                       return $ x * y
evalA (Div a1 a2) = do x <- evalA a1
                       y <- evalA a2
                       if y == 0
                         then fail "Division by zero"
                         else return $ x `div` y


-- | The evaluator for boolean expressions
evalB :: (MonadFail m, MonadState Env m) => Bexp -> m Bool
evalB BTrue       = return True
evalB BFalse      = return False
evalB (Eq a1 a2)  = do x <- evalA a1
                       y <- evalA a2
                       return $ x == y
evalB (Le a1 a2)  = do x <- evalA a1
                       y <- evalA a2
                       return $ x <= y
evalB (Not b)     = do x <- evalB b
                       return (not x)
evalB (And b1 b2) = do x <- evalB b1
                       y <- evalB b2
                       return $ x && y

-- | The evaluator for statements
evalS :: (Alternative m, MonadFail m, MonadState Env m) => Stm -> m ()
evalS (Assign v a) = do x <- evalA a
                        setVar v x
evalS Skip         = return ()
evalS (Seq s1 s2)  = evalS s1 >> evalS s2
evalS (If b s1 s2) = do boolVal <- evalB b
                        if boolVal
                          then evalS s1
                          else evalS s2
evalS (While b s) = do 
                      boolVal <- evalB b
                      if boolVal
                          then evalS s >> evalS (While b s)
                          else return ()
evalS (Or s1 s2) = evalS s1 <|> evalS s2


-- | Run a While program using big-step semantics
runBig :: String -> IO Env
runBig s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> case runWhile (evalS stm) emptyEnv of
                  Left msg        -> fail msg
                  Right ((), env) -> return env

-- | Run a While program using big-step semantics non-deterministically
runBigPar :: String -> IO [Either String Env]
runBigPar s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> return $ map (fmap snd) (runParWhile (evalS stm) emptyEnv)
