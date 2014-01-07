{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Extra where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.STM
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
import Data.IORef
import Data.Maybe (catMaybes)

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
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

-- | A monadic version of @mapMaybe :: (a -> Maybe b) -> [a] -> [b]@.
mapMaybeM :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

-- | A transformer-friendly version of 'atomically'.
atomicallyM :: MonadIO m => STM a -> m a
atomicallyM = liftIO . atomically

-- | Embed a transformer (Kleisli) arrow as an arrow in the base monad
--   returning a mutated transformer state.  If you do not want the
--   transformation and your base monad is IO, use 'embedIO'.
embed :: (MonadBaseControl base m) => (a -> m b) -> m (a -> base (StM m b))
embed f = control $ \run -> run $ return (run . f)

-- | Return an IO action that closes over the current monad transformer, but
--   throws away any residual effects within that transformer.
embedIO :: (MonadBaseControl IO m, MonadIO m) => (a -> m b) -> m (a -> IO b)
embedIO f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a -> do
        _ <- run $ do
             res <- f a
             liftIO $ writeIORef result res
        readIORef result

embedIO2 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> m r) -> m (a -> b -> IO r)
embedIO2 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b -> do
        _ <- run $ do
             res <- f a b
             liftIO $ writeIORef result res
        readIORef result

embedIO3 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> m r) -> m (a -> b -> c -> IO r)
embedIO3 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c -> do
        _ <- run $ do
             res <- f a b c
             liftIO $ writeIORef result res
        readIORef result

embedIO4 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> m r) -> m (a -> b -> c -> d -> IO r)
embedIO4 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d -> do
        _ <- run $ do
             res <- f a b c d
             liftIO $ writeIORef result res
        readIORef result

embedIO5 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> m r) -> m (a -> b -> c -> d -> e -> IO r)
embedIO5 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e -> do
        _ <- run $ do
             res <- f a b c d e
             liftIO $ writeIORef result res
        readIORef result

embedIO6 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> m r)
          -> m (a -> b -> c -> d -> e -> f -> IO r)
embedIO6 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f -> do
        _ <- run $ do
             res <- x a b c d e f
             liftIO $ writeIORef result res
        readIORef result

embedIO7 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> IO r)
embedIO7 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g -> do
        _ <- run $ do
             res <- x a b c d e f g
             liftIO $ writeIORef result res
        readIORef result

embedIO8 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> h -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> h -> IO r)
embedIO8 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g h -> do
        _ <- run $ do
             res <- x a b c d e f g h
             liftIO $ writeIORef result res
        readIORef result

embedIO9 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> h -> i -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO r)
embedIO9 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g h i -> do
        _ <- run $ do
             res <- x a b c d e f g h i
             liftIO $ writeIORef result res
        readIORef result

-- | Draw monadic actions from a list until one of them yields a value
--   satisfying the predicate, and then return all the values up to and
--   including the first that succeeds in a list within that monad.
sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ [] = return []
sequenceUntil p (m:ms) = do
    a <- m
    if p a
        then return [a]
        else do
            as <- sequenceUntil p ms
            return (a:as)

-- | Draw monadic actions from a list until one of them yields a value
--   failing the predicate, and then return all the passing values
--   (discarding the final, failing value) in a list within that
--   monad.
sequenceWhile :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile _ [] = return []
sequenceWhile p (m:ms) = do
    a <- m
    if p a
        then do
            as <- sequenceWhile p ms
            return (a:as)
        else return []
