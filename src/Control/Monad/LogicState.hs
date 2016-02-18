{-# LANGUAGE UndecidableInstances, Rank2Types, FlexibleInstances, FlexibleContexts, GADTs, ScopedTypeVariables, FunctionalDependencies #-}

-------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.LogicState
-- Copyright   : (c) Atze Dijkstra
-- License     : BSD3
--
-- Maintainer  : atzedijkstra@gmail.com
-- Stability   : experimental
-- Portability : non-portable (multi-parameter type classes)
--
-- A backtracking, logic programming monad on top of logict, adding backtrackable state
--
--    Adapted from the paper
--    /Backtracking, Interleaving, and Terminating
--        Monad Transformers/, by
--    Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry
--    (<http://www.cs.rutgers.edu/~ccshan/logicprog/LogicT-icfp2005.pdf>).
-------------------------------------------------------------------------

module Control.Monad.LogicState (
    module Control.Monad.Logic.Class,
    module Control.Monad,
    module Control.Monad.Trans,
    module Control.Monad.LogicState.Class,
    module Control.Monad.TransLogicState.Class,
    -- * The LogicVar monad
    -- LogicVar,
    LogicState,
    {-
    logicVar,
    runLogicVar,
    -- * The LogicVarT monad transformer
    -}
    -- LogicVarT(..),
    {-
    runLogicVarT,
    -}
    LogicStateT(..),
  ) where

import Data.Maybe
import Data.Typeable

import Control.Applicative

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans

import Control.Monad.State
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class

import Data.Monoid (Monoid(mappend, mempty))
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Control.Monad.Logic.Class

import Control.Monad.LogicState.Class
import Control.Monad.TransLogicState.Class

-------------------------------------------------------------------------
-- | A monad transformer for performing backtracking computations
-- layered over another monad 'm', with propagation of global and backtracking state, e.g. resp. for freshness/uniqueness and maintaining variable mappings.
newtype LogicStateT gs bs m a =
    LogicStateT { unLogicStateT ::
      forall r. LogicContS gs bs r m a
    }

-- | Convenience types
type LogicStateS gs bs r m = StateT (gs,bs) m r -- (gs,bs) -> m (r,(gs,bs))
type LogicContS gs bs r m a =
           (   a                                 --  result
            -> LogicStateS gs bs r m             --  failure continuation
            -> LogicStateS gs bs r m
           )                                     -- ^ success continuation
        -> LogicStateS gs bs r m                 -- ^ failure continuation
        -> LogicStateS gs bs r m                 -- ^ global + backtracking state

instance Functor (LogicStateT gs bs f) where
    fmap f lt = LogicStateT $ \sk -> unLogicStateT lt (sk . f)

instance Applicative (LogicStateT gs bs f) where
    pure a = LogicStateT $ \sk -> sk a
    f <*> a = LogicStateT $ \sk -> unLogicStateT f (\g -> unLogicStateT a (sk . g))

instance Monad (LogicStateT gs bs m) where
    return a = LogicStateT ($ a)
    m >>= f = LogicStateT $ \sk -> unLogicStateT m (\a -> unLogicStateT (f a) sk)
    fail _ = LogicStateT $ flip const

instance Alternative (LogicStateT gs bs f) where
    empty = LogicStateT $ flip const
    -- state backtracking variant, but in general interacts badly with other combinators using msplit. Backtracking separately available.
    -- f1 <|> f2 = LogicStateT $ \sk fk -> StateT $ \s@(_,bs) -> runStateT (unLogicStateT f1 sk (StateT $ \(gs',_) -> runStateT (unLogicStateT f2 sk fk) (gs',bs))) s
    f1 <|> f2 = LogicStateT $ \sk fk -> unLogicStateT f1 sk (unLogicStateT f2 sk fk)

instance MonadPlus (LogicStateT gs bs m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadTrans (LogicStateT gs bs) where
    lift m = LogicStateT $ \sk fk -> StateT $ \s -> m >>= \a -> runStateT (sk a fk) s

instance (MonadIO m) => MonadIO (LogicStateT gs bs m) where
    liftIO = lift . liftIO

instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (LogicStateT gs bs m) where
    get = lift get
    put = lift . put

instance MonadReader r m => MonadReader r (LogicStateT gs bs m) where
    ask = lift ask
    local f m = LogicStateT $ \sk fk -> StateT $ runStateT $ unLogicStateT m (\a fk -> StateT $ local f . runStateT (sk a fk)) (StateT $ local f . runStateT fk)

{-
instance MonadError e m => MonadError e (LogicStateT gs bs m) where
  throwError = lift . throwError
  catchError m h = LogicStateT $ \sk fk -> StateT $ \s -> let
      handle r = r `catchError` \e -> put s >> unLogicStateT (h e) sk fk
    in handle $ put s >> unLogicStateT m (\a fk' -> sk a (handle . fk')) fk
-}

{-
instance MonadError e m => MonadError e (LogicStateT gs bs m) where
  throwError = lift . throwError
  catchError m h = LogicStateT $ \sk fk -> StateT $ \s -> let
      handle r = r `catchError` \e -> StateT $ \_ -> runStateT (unLogicStateT (h e) sk fk) s
    in handle $ StateT $ \_ -> runStateT (unLogicStateT m (\a fk' -> sk a (handle . fk')) fk) s
-}

{-
-}
instance (Monad m) => MonadLogic (LogicStateT gs bs m) where
    msplit m =
       liftWithState $ runStateT $ unLogicStateT m
         (\a fk -> return (Just (a, liftWithState (runStateT fk) >>= reflect)))
         (return Nothing)

instance TransLogicState (gs,bs) (LogicStateT gs bs) where
  observeT s lt = evalStateT (unLogicStateT lt (\a _ -> return a) (fail "No answer.")) s
  
  observeAllT s m = evalStateT (unLogicStateT m
    (\a fk -> fk >>= \as -> return (a:as))
    (return []))
    s
  
  observeManyT s n m = evalStateT (obs n m) s
   where
     obs n m
        | n <= 0 = return []
        | n == 1 = unLogicStateT m (\a _ -> return [a]) (return [])
        | otherwise = unLogicStateT (msplit m) sk (return [])
     
     sk Nothing _ = return []
     sk (Just (a, m')) _ = StateT $ \s -> (\as -> (a:as,s)) `liftM` observeManyT s (n-1) m'

  liftWithState m = LogicStateT $ \sk fk -> StateT $ \s -> m s >>= \(a,s) -> runStateT (sk a fk) s
  {-# INLINE liftWithState #-}

instance Monad m => MonadState (gs,bs) (LogicStateT gs bs m) where
    get   = LogicStateT $ \sk fk -> get >>= \s -> sk s fk
    put s = LogicStateT $ \sk fk -> put s >>= \a -> sk a fk

instance (Monad m) => MonadLogicState (gs,bs) (LogicStateT gs bs m) where
    backtrack m = get >>= \(_::gs,bs) -> return $ LogicStateT $ \sk fk -> StateT $ \(gs,_) -> runStateT (unLogicStateT m sk fk) (gs,bs)


-------------------------------------------------------------------------
-- | The basic LogicVar monad, for performing backtracking computations
-- returning values of type 'a'
type LogicState gs bs = LogicStateT gs bs Identity

{-
-------------------------------------------------------------------------
-- | A monad transformer for performing backtracking computations
-- layered over another monad 'm', with propagation of global and backtracking state, e.g. resp. for freshness/uniqueness and maintaining variable mappings.
newtype LogicVarT gs bs m a =
    LogicVarT { unLogicVarT ::
      forall r. {- (Typeable r) => -} LogicCont gs bs r m a
    }

-- | Convenience types
type LogicStateT gs bs r m = (gs,bs) -> m (r,(gs,bs)) -- StateT (gs,bs) m r -- (gs,bs) -> m (r,(gs,bs))
type LogicCont gs bs r m a =
           (   a                                -- ^ result
            -> LogicState gs bs r m             -- ^ failure continuation
            -> LogicState gs bs r m
           )                                    -- ^ success continuation
        -> LogicState gs bs r m                 -- ^ failure continuation
        -> LogicState gs bs r m                 -- ^ global + backtracking state

instance Functor (LogicVarT gs bs f) where
    fmap f lt = LogicVarT $ \sk -> unLogicVarT lt (sk . f)

instance Applicative (LogicVarT gs bs f) where
    pure a = LogicVarT $ \sk -> sk a
    f <*> a = LogicVarT $ \sk -> unLogicVarT f (\g -> unLogicVarT a (sk . g))

instance Monad (LogicVarT gs bs m) where
    return a = LogicVarT $ \sk -> sk a
    m >>= f = LogicVarT $ \sk -> unLogicVarT m (\a -> unLogicVarT (f a) sk)
    fail _ = LogicVarT $ \_ fk -> fk

instance Alternative (LogicVarT gs bs f) where
    empty = LogicVarT $ \_ fk -> fk
    f1 <|> f2 = LogicVarT $ \sk fk s@(_,bs) -> unLogicVarT f1 sk (\(gs',_) -> unLogicVarT f2 sk fk (gs',bs)) s

instance MonadPlus (LogicVarT gs bs m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadTrans (LogicVarT gs bs) where
    lift m = LogicVarT $ \sk fk s -> m >>= \a -> sk a fk s

instance (MonadIO m) => MonadIO (LogicVarT gs bs m) where
    liftIO = lift . liftIO
-}

{-
data ResultLV gs bs r m a where
    DoneR :: ResultLV gs bs r m a
    NextR :: a -> LogicCont gs bs r m a -> ResultLV gs bs r m a
-}

{-
instance (Monad m, F.Foldable m) => F.Foldable (LogicVarT m) where
    foldMap f m = F.fold $ unLogicVarT m (liftM . mappend . f) (return mempty)

instance T.Traversable (LogicVarT Identity) where
    traverse g l = runLogicVar l (\a ft -> cons <$> g a <*> ft) (pure mzero)
     where cons a l' = return a `mplus` l'
-}

{-
-- Needs undecidable instances
instance MonadReader r m => MonadReader r (LogicVarT gs bs m) where
    ask = lift ask
    local f m = LogicVarT $ \sk fk -> unLogicVarT m (\a fk -> local f . sk a fk) (local f . fk)
    -- ((local f .) . sk) (local f fk)
    -- (\a -> (local f .) $ \fk -> sk a fk) (local f fk)

-- Needs undecidable instances
instance MonadState s m => MonadState s (LogicVarT gs bs m) where
    get = lift get
    put = lift . put

-- Needs undecidable instances
instance MonadError e m => MonadError e (LogicVarT gs bs m) where
  throwError = lift . throwError
  catchError m h = LogicVarT $ \sk fk s -> let
      handle r = r `catchError` \e -> unLogicVarT (h e) sk fk s
    in handle $ unLogicVarT m (\a fk' -> sk a (handle . fk')) fk s
-}
{-
  catchError m h = LogicT $ \sk fk -> let
      handle r = r `catchError` \e -> unLogicT (h e) sk fk
    in handle $ unLogicT m (\a -> sk a . handle) fk
-}

{-
instance (Monad m) => MonadLogic (LogicVarT gs bs m) where
    msplit m =
       liftWithState $ unLogicVarT m
         (\a fk s -> return (Just (a, liftWithState fk >>= reflect), s))
         (\s -> return (Nothing,s))
-}
{-
    msplit m =
       liftWithState $ \s -> unLogicVarT m s
         (\a s2@(gs2,bs2) fk -> return
           ( Just ( a
                  , do ma <- liftWithState fk -- $ \s3@(gs3,bs3::bs) -> fk s3 -- >>= \(a,s@(gs,bs)) -> return (a,s))
                       reflect ma
                  )
           , s2
           ))
         (\s -> return (Nothing,s))
-}
{-
    interleave m1 m2 = msplit m1 >>=
                        maybe m2 (\(a, m1') -> return a `mplus` interleave m2 m1')

    m >>- f = do (a, m') <- maybe mzero return =<< msplit m
                 interleave (f a) (m' >>- f)

    ifte t th el = msplit t >>= maybe el (\(a,m) -> th a `mplus` (m >>= th))

    once m = do (a, _) <- maybe mzero return =<< msplit m
                return a
-}

{-
instance (Monad m) => MonadLogicState (gs,bs) (LogicVarT gs bs m) where
    lvGet = LogicVarT $ \sk fk s -> sk s fk s
    lvModifyGet f = LogicVarT $ \sk fk s -> let (x,s') = f s in sk x fk s'

instance TransLogicState (gs,bs) (LogicVarT gs bs) where
  -------------------------------------------------------------------------
  -- | Extracts the first result from a LogicVarT computation,
  -- failing otherwise.
  observeT s lt = fmap fst $ unLogicVarT lt (\a _ s -> return (a,s)) (\_ -> fail "No answer.") s
  
  -------------------------------------------------------------------------
  -- | Extracts all results from a LogicVarT computation.
  observeAllT s m = fmap fst $ unLogicVarT m
    (\a fk s -> fk s >>= \(as,s') -> return (a:as, s'))
    (\s -> return ([],s))
    s
  
  -------------------------------------------------------------------------
  -- | Extracts up to a given number of results from a LogicVarT computation.
  observeManyT s n m = fmap fst $ obs s n m
   where
     obs s n m
        | n <= 0 = return ([],s)
        | n == 1 = unLogicVarT m (\a _ s -> return ([a],s)) (\s -> return ([],s)) s
        | otherwise = unLogicVarT (msplit m) sk (\s -> return ([],s)) s
     
     sk Nothing _ s = return ([],s)
     sk (Just (a, m')) _ s = (\as -> (a:as,s)) `liftM` observeManyT s (n-1) m'

  -- |
  liftWithState m = LogicVarT $ \sk fk s -> m s >>= \(a,s) -> sk a fk s
-}

{-
  
-------------------------------------------------------------------------
-- | Runs a LogicVarT computation with the specified initial success and
-- failure continuations.
runLogicVarT :: LogicVarT m a -> (a -> m r -> m r) -> m r -> m r
runLogicVarT = unLogicVarT
-}

{-
-------------------------------------------------------------------------
-- | The basic LogicVar monad, for performing backtracking computations
-- returning values of type 'a'
type LogicVar gs bs = LogicVarT gs bs Identity

-------------------------------------------------------------------------
-- | A smart constructor for LogicVar computations.
logicVar :: (forall r. (a -> r -> r) -> r -> r) -> LogicVar a
logicVar f = LogicVarT $ \k -> Identity .
                         f (\a -> runIdentity . k a . Identity) .
                         runIdentity

-------------------------------------------------------------------------
-- | Runs a LogicVar computation with the specified initial success and
-- failure continuations.
runLogicVar :: LogicVar a -> (a -> r -> r) -> r -> r
runLogicVar l s f = runIdentity $ unLogicVarT l si fi
 where
 si = fmap . s
 fi = Identity f

-}

