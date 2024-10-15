{-# OPTIONS_GHC -fwarn-tabs #-}

module While.Small (
    stepS,
    stepsS,
    runSmall,
    runSmallPar
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad.State (MonadState)

import While.Big (evalA, evalB)
import While.Env (Env, emptyEnv, setVar)
import While.Monad (runWhile)
import While.ParMonad (runParWhile)
import While.Parser (parseWhile)
import While.Syntax (Stm(..))

-- | Small-step semantics for statements
stepS :: (Alternative m, MonadFail m, MonadState Env m) => Stm -> m (Maybe Stm)
stepS (Assign v a) = do x <- evalA a
                        setVar v x
                        return Nothing
stepS Skip         = return Nothing
stepS (Seq s1 s2)  = do maybe_s1' <- stepS s1
                        case maybe_s1' of
                          Just s1' -> return (Just (Seq s1' s2))
                          Nothing  -> return (Just s2)
stepS (If b s1 s2) = do
    boolVal <- evalB b
    if boolVal then return (Just s1) else return (Just s2)
stepS (While b s) = do
    boolVal <- evalB b
    if boolVal then return (Just (Seq s (While b s))) else return Nothing
stepS (Or s1 s2) = stepFirst s1 s2 <|> stepSecond s1 s2
  where
    stepFirst s1 s2 = do
      maybe_s1' <- stepS s1
      case maybe_s1' of
        Just s1' -> return (Just (Or s1' s2))
        Nothing  -> return (Just s2)

    stepSecond s1 s2 = do
      maybe_s2' <- stepS s2
      case maybe_s2' of
        Just s2' -> return (Just (Or s1 s2'))
        Nothing  -> return (Just s1)

--stepS (Or s1 s2) = return (Just s1) <|> return (Just s2)

-- | Run @stepS@ until it can no longer step
stepsS :: (Alternative m, MonadFail m, MonadState Env m) => Stm -> m ()
stepsS s = do maybe_s' <- stepS s
              case maybe_s' of
                Nothing -> return ()
                Just s' -> stepsS s'

-- | Run a While program using small-step semantics
runSmall :: String -> IO Env
runSmall s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> case runWhile (stepsS stm) emptyEnv of
                  Left msg        -> fail msg
                  Right ((), env) -> return env

-- | Run a While program using small-step semantics non-deterministically
runSmallPar :: String -> IO [Either String Env]
runSmallPar s =
  case parseWhile s of
    Nothing  -> fail $ "Cannot parse: " ++ s
    Just stm -> return $ map (fmap snd) (runParWhile (stepsS stm) emptyEnv)
