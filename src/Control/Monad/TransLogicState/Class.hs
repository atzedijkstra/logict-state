{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

module Control.Monad.TransLogicState.Class
  ( TransLogicState(..)
  
  , observe
  , observeAll
  , observeMany
  )
  where

import Data.Typeable

import Control.Monad.Identity

class TransLogicState extra t where
  -------------------------------------------------------------------------
  -- | Extracts the first result from a 't m' computation,
  -- failing otherwise.
  observeT :: (Monad m{- , Typeable a, Typeable m, Typeable extra -}) => extra -> t m a -> m a
  observeT e m = fmap head $ observeManyT e 1 m
  
  -------------------------------------------------------------------------
  -- | Extracts all results from a 't m' computation.
  observeAllT :: (Monad m{- , Typeable a, Typeable m, Typeable extra -})=> extra -> t m a -> m [a]
  observeAllT e = observeManyT e maxBound
  
  -------------------------------------------------------------------------
  -- | Extracts up to a given number of results from a 't m' computation.
  observeManyT :: forall m a . (Monad m{- , Typeable a, Typeable m, Typeable extra -}) => extra -> Int -> t m a -> m [a]
  observeManyT e n m = fmap (take n) $ observeAllT e m

  -- |
  liftWithState :: Monad m => (extra -> m (a,extra)) -> t m a

-------------------------------------------------------------------------
-- | Extracts the first result from a LogicVar computation.
observe :: (TransLogicState extra t{- , Typeable a, Typeable extra -}) => extra -> t Identity a -> a
observe e = runIdentity . observeT e

-------------------------------------------------------------------------
-- | Extracts all results from a LogicVar computation.
observeAll :: (TransLogicState extra t{- , Typeable a, Typeable extra -}) => extra -> t Identity a -> [a]
observeAll e = runIdentity . observeAllT e

-------------------------------------------------------------------------
-- | Extracts up to a given number of results from a LogicVar computation.
observeMany :: (TransLogicState extra t{- , Typeable a, Typeable extra -}) => extra -> Int -> t Identity a -> [a]
observeMany e i = runIdentity . observeManyT e i

{-
-------------------------------------------------------------------------------
-- | API for MonadLogic which allows extra info/state.
-- Minimal implementation: msplit
class (MonadPlus m) => MonadLogicVar m where
    -- | Attempts to split the computation, giving access to the first
    --   result. Satisfies the following laws:
    --
    --   > msplit mzero                == return Nothing
    --   > msplit (return a `mplus` m) == return (Just (a, m))
    msplit     :: m a -> m (Maybe (a, m a))

    -- | Fair disjunction. It is possible for a logical computation
    --   to have an infinite number of potential results, for instance:
    --
    --   > odds = return 1 `mplus` liftM (2+) odds
    --
    --   Such computations can cause problems in some circumstances. Consider:
    --
    --   > do x <- odds `mplus` return 2
    --   >    if even x then return x else mzero
    --
    --   Such a computation may never consider the 'return 2', and will
    --   therefore never terminate. By contrast, interleave ensures fair
    --   consideration of both branches of a disjunction
    interleave :: m a -> m a -> m a

    -- | Fair conjunction. Similarly to the previous function, consider
    --   the distributivity law for MonadPlus:
    --
    --   > (mplus a b) >>= k = (a >>= k) `mplus` (b >>= k)
    --
    --   If 'a >>= k' can backtrack arbitrarily many tmes, (b >>= k) may never
    --   be considered. (>>-) takes similar care to consider both branches of
    --   a disjunctive computation.
    (>>-)      :: m a -> (a -> m b) -> m b
    infixl 1 >>-

    -- | Logical conditional. The equivalent of Prolog's soft-cut. If its
    --   first argument succeeds at all, then the results will be fed into
    --   the success branch. Otherwise, the failure branch is taken.
    --   satisfies the following laws:
    --
    --   > ifte (return a) th el           == th a
    --   > ifte mzero th el                == el
    --   > ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
    ifte       :: m a -> (a -> m b) -> m b -> m b

    -- | Pruning. Selects one result out of many. Useful for when multiple
    --   results of a computation will be equivalent, or should be treated as
    --   such.
    once       :: m a -> m a

    -- All the class functions besides msplit can be derived from msplit, if
    -- desired
    interleave m1 m2 = msplit m1 >>=
                        maybe m2 (\(a, m1') -> return a `mplus` interleave m2 m1')

    m >>- f = do (a, m') <- maybe mzero return =<< msplit m
                 interleave (f a) (m' >>- f)

    ifte t th el = msplit t >>= maybe el (\(a,m) -> th a `mplus` (m >>= th))

    once m = do (a, _) <- maybe mzero return =<< msplit m
                return a
-}
