module API.Prelude(
    Functor(..)
  , Applicative(..)
  , Monad(..)
  , Int
  , Double
  , Bool
  , RIO
  , Eq(..)
  , Ord(..)
  , getNumber
  -- * For trusted side
  , runRIO
  ) where

import Prelude
import Control.Monad
import Control.Applicative
import API.Input

newtype RIO a = RIO { runRIO :: InputAPI -> IO a }

instance Functor RIO where
  fmap f (RIO ma) = RIO $ fmap f . ma
  {-# INLINE fmap #-}

instance Applicative RIO where
  pure a = RIO $ const $ pure a
  mf <*> ma = RIO $ \a -> runRIO mf a <*> runRIO ma a
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad RIO where
  return = pure
  ma >>= mf = RIO $ \v -> do
    a <- runRIO ma v
    runRIO (mf a) v
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

getNumber :: RIO Int
getNumber = RIO $ \InputAPI{..} -> _getNumber
