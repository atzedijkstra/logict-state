{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Control.Monad.LogicState.Class
  ( MonadLogicState(..)
  )
  where

import Control.Monad
import Control.Monad.Logic.Class

-------------------------------------------------------------------------------
-- | API for MonadLogic which allows state and backtracking on it
-- Minimal implementation: msplit
class (MonadLogic m) => MonadLogicState s m | m -> s where
    -- |
    lvGet :: m s
    -- | update, get old one
    lvModifyGet :: (s -> (a,s)) -> m a
    -- |
    -- lvWith :: s -> m a -> m (a,s)
    -- |
    -- lvExtra :: m a -> m (a,s)
    
    -- | Return argument monad with the current backtrackable part of the state remembered
    backtrack :: m a -> m (m a)
    backtrack = return

    -- | Attempts to split the computation, giving access to the first
    --   result. Satisfies the following laws:
    --
    --   > msplit mzero                == return Nothing
    --   > msplit (return a `mplus` m) == return (Just (a, m))
    -- msplitLV     :: m a -> m (Maybe ((a,s), m a))

 {-
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
    interleaveLV :: s -> m a -> m a -> m a

    -- | Fair conjunction. Similarly to the previous function, consider
    --   the distributivity law for MonadPlus:
    --
    --   > (mplus a b) >>= k = (a >>= k) `mplus` (b >>= k)
    --
    --   If 'a >>= k' can backtrack arbitrarily many tmes, (b >>= k) may never
    --   be considered. (>>-) takes similar care to consider both branches of
    --   a disjunctive computation.
    bindLV      :: s -> m a -> (a -> m b) -> m b
    -- infixl 1 >>-

    -- | Logical conditional. The equivalent of Prolog's soft-cut. If its
    --   first argument succeeds at all, then the results will be fed into
    --   the success branch. Otherwise, the failure branch is taken.
    --   satisfies the following laws:
    --
    --   > ifte (return a) th el           == th a
    --   > ifte mzero th el                == el
    --   > ifte (return a `mplus` m) th el == th a `mplus` (m >>= th)
    ifteLV       :: s -> m a -> (a -> m b) -> m b -> m b

    -- | Pruning. Selects one result out of many. Useful for when multiple
    --   results of a computation will be equivalent, or should be treated as
    --   such.
    onceLV       :: s -> m a -> m a

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
