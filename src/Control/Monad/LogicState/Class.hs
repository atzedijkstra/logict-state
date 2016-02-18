{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.LogicState.Class
  ( MonadLogicState(..)
  )
  where

import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.State

-------------------------------------------------------------------------------
-- | API for MonadLogic which allows state and backtracking on it
-- Minimal implementation: msplit
class (MonadLogic m, MonadState s m) => MonadLogicState s m where
    
    -- | Return argument monad with the current backtrackable part of the state remembered.
    -- If the default def is not overridden this a no-op.
    -- This function complements 'mplus' for 'LogicT', 'mplus' backtracks on results, not on state, which is what this function should do.
    backtrack :: m a -> m (m a)
    backtrack = return

