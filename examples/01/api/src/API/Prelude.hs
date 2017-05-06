module API.Prelude(
    Functor(..)
  , Applicative(..)
  , Monad(..)
  , Int
  , Double
  , Bool
  , RIO
  ) where

import Control.Monad

newtype RIO a = RIO { runRIO :: IO a }

instance Functor RIO where
  fmap f (RIO ma) = RIO $ fmap f ma
  {-# INLINE fmap #-}

instance Applicative RIO where
  pure a = RIO $ pure a
  mf <*> ma = RIO $ runRIO mf <*> runRIO ma
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad RIO where
  return = pure
  ma >>= mf = RIO $ do
    a <- runRIO ma
    runRIO $ mf a
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
