module Control.Monad.Extra where

import Control.Applicative
import Control.Monad.Trans.Cont

-- | Synonym for @return ()@.
skip :: Monad m => m ()
skip = return ()

-- | Synonym for @pure ()@.
obvious :: Applicative f => f ()
obvious = pure ()

-- | Function name for '>>=', as fmap is to '<$>'.
bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

-- | Combinator for working with monadic values:
--
-- >>> om when (return True) $ print "Hello"
-- "Hello"
-- >>> return True >>= flip when (print "Hello")
-- "Hello"
-- >>> om forM_ (return [True]) print
-- True
-- >>> flip forM_ print =<< return [True]
-- True
-- >>> mapM_ print =<< return [True]
-- True
--
-- This subsumes the need for individual functions for 'whenM', 'unlessM',
-- etc.
om :: Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m = (m >>=) . flip f

-- | Variant of 'om' which changes the roles of the 2nd and 3rd arguments.
--
-- >>> nom mapM_ print $ return [True]
-- True
-- >>> mapM_ print =<< return [True]
-- True
nom :: Monad m => (a -> b -> m c) -> a -> m b -> m c
nom f x m = m >>= f x

-- | Convenience functions if all you want to use is 'callCC'.
doCallCC :: Monad m => ((r -> ContT r m b) -> ContT r m r) -> m r
doCallCC = flip runContT return . callCC
