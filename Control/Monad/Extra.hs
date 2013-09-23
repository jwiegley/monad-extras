module Control.Monad.Extra where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class

-- | Synonym for @return ()@.
skip :: Monad m => m ()
skip = return ()

-- | Discards a value
discard :: Monad m => a -> m ()
discard _ = return ()

-- | Synonym for @pure ()@.
obvious :: Applicative f => f ()
obvious = pure ()

-- | Function name for '>>=', as 'fmap' is to '<$>'.
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
-- Subsumes the need for individual functions for 'whenM', 'unlessM', etc.
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

-- | Convenience function if all you want to use is
--   'Control.Monad.Trans.Cont.callCC'.
doCallCC :: Monad m => ((r -> ContT r m b) -> ContT r m r) -> m r
doCallCC = flip runContT return . callCC

-- | Return a continuation that one can jump back to within 'ContT'.
--
-- >>> flip runContT return $ do { k <- label; ...; k }
label :: ContT r m (ContT r m a)
label = callCC $ \k -> let m = k m in return m

-- | Short-hand for @liftIO@.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Lift a 'Maybe' value into the 'MaybeT' monad transformer.
liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
