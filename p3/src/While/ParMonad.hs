{-# OPTIONS_GHC -fwarn-tabs #-}

module While.ParMonad (
    runParWhile
) where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad.State (MonadPlus, MonadState(get, put))

import While.Env (Env)

-- XXX We really should use monad transformers here.

-- | A monad with an @Env@ state that handles failure and nondeterminism.
newtype ParWhile a = ParWhile (Env -> [Either String (a, Env)])

runParWhile :: ParWhile a -> Env -> [Either String (a, Env)]
runParWhile (ParWhile f) = f

instance Functor ParWhile where
    fmap f m = ParWhile $ \env -> do either_x <- runParWhile m env
                                     case either_x of
                                       Left msg        -> return $ Left msg
                                       Right (x, env') -> return $ Right (f x , env')
instance Applicative ParWhile where
    pure x = ParWhile $ \env -> return $ Right (x, env)

    mf <*> mx = ParWhile $ \env -> do either_f <- runParWhile mf env
                                      case either_f of
                                        Left msg        -> return $ Left msg
                                        Right (f, env') -> do either_x <- runParWhile mx env'
                                                              case either_x of
                                                                Left msg         -> return $ Left msg
                                                                Right (x, env'') -> return $ Right (f x, env'')

instance Alternative ParWhile where
   empty = ParWhile $ \_ -> return $ Left "empty"

   p <|> q = ParWhile $ \env -> runParWhile p env ++ runParWhile q env

instance Monad ParWhile where
    mx >>= f = ParWhile $ \env -> do either_x <- runParWhile mx env
                                     case either_x of
                                       Left msg        -> return $ Left msg
                                       Right (x, env') -> runParWhile (f x) env'

instance MonadFail ParWhile where
    fail ms = ParWhile $ \_ -> return $ Left ms

instance MonadPlus ParWhile where

instance MonadState Env ParWhile where
    get = ParWhile $ \env -> return $ Right (env, env)

    put env = ParWhile $ \_ -> return $ Right ((), env)
