module Game.Crafty.Script(
  -- * Monad
    ScriptT
  , runScriptT
  , InterpreterError(..)
  , GhcError(..)
  , MonadScript(..)
  -- * API
  , Extension(..)
  , setExtensions
  , loadScriptPackage
  , execScript
  ) where

import Data.Typeable
import Language.Haskell.Interpreter

data ScriptT (m :: * -> *) a
  -- deriving (Functor, Applicative, Monad)

instance Functor m => Functor (ScriptT m)
instance Applicative m => Applicative (ScriptT m)
instance Monad m => Monad (ScriptT m)

runScriptT :: ScriptT m a -> m (Either InterpreterError a)
runScriptT = undefined

class MonadScript (m :: * -> *)

instance MonadScript (ScriptT m)

setExtensions :: MonadScript m
  => [Extension]
  -> m ()
setExtensions = undefined

loadScriptPackage :: MonadScript m
  => FilePath
  -> m ()
loadScriptPackage = undefined

execScript :: (MonadScript m, Typeable a)
  => String
  -> m a
execScript = undefined
