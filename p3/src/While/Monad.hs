{-# OPTIONS_GHC -fwarn-tabs #-}

module While.Monad where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad.State (MonadPlus, MonadState(get, put))

import While.Env

-- | A monad with an @Env@ state that handles failure.
newtype While a = While (Env -> Either String (a, Env))

runWhile :: While a -> Env -> Either String (a, Env)
runWhile (While f) = f

instance Functor While where
    fmap f m = While $ \env -> do (x, env') <- runWhile m env
                                  return (f x, env')

instance Applicative While where
    pure x = While $ \env -> Right (x, env)

    mf <*> mx = While $ \env -> do (f, env')  <- runWhile mf env
                                   (x, env'') <- runWhile mx env'
                                   return (f x, env'')

instance Alternative While where
   empty = While $ \_ -> Left "empty"

   p <|> q = While $ \env -> case runWhile p env of
                               Left _     -> runWhile q env
                               Right env' -> Right env'

instance Monad While where
    mx >>= f = While $ \s -> do (x, s') <- runWhile mx s
                                runWhile (f x) s'

instance MonadFail While where
    fail ms = While $ \_ -> Left ms

instance MonadPlus While where

instance MonadState Env While where
    get = While $ \env -> Right (env, env)

    put env = While $ \_ -> Right ((), env)
