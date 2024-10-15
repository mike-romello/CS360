module While.Env (
    Env(..),
    emptyEnv,
    lookupVar,
    setVar
  ) where

import Control.Monad.State (MonadState(put, get))

import While.Syntax (Var, Z)

-- | In lecture, we represented the state as a function. Here we represent state
-- as an association list instead.
newtype Env = Env [(Var, Z)]
  deriving (Eq, Ord, Show)

-- | The empty state contains no bindings for any variables---it as an empty
-- association list.
emptyEnv :: Env
emptyEnv = Env []

-- | 'lookupVar' finds the value associated with a variable in the current
-- environment.
lookupVar :: (MonadFail m, MonadState Env m) => Var -> m Z
lookupVar v = do Env bindings <- get
                 go bindings
  where
    go :: MonadFail m => [(Var, Z)] -> m Z
    go []                      = fail $ "Unbound variable " ++ show v
    go ((v',n):vs) | v' == v   = return n
                   | otherwise = go vs

-- | 'setVar' sets the value of a variable. To avoid an exploding association
-- list, we prune any previous binding for @v@.
setVar :: MonadState Env m => Var -> Z -> m ()
setVar v n = do Env bindings <- get
                put $ Env $ (v,n) : filter (\(v', _) -> v' /= v) bindings
