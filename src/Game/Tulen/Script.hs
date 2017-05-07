module Game.Tulen.Script(
  -- * Monad
    ScriptT
  , runScriptT
  , ScriptError(..)
  , InterpreterError(..)
  , GhcError(..)
  , MonadScript(..)
  -- * API
  , Extension(..)
  , setExtensions
  , loadScriptPackage
  , execScript
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (get)
import Data.Bifunctor
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (unpack)
import Data.Typeable
import GHC.Generics
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory
import System.FilePath

import Game.Tulen.Script.Package

import qualified Data.Foldable as F
import qualified Data.Sequence as S

class MonadInterpreter m => MonadScript m where
  addScriptModules :: Foldable f => f ModuleName -> m ()
  getScriptModules :: m [ModuleName]

  addTopLevelModule :: Foldable f => f ModuleName -> m ()
  getTopLevelModules :: m [ModuleName]

data ScriptError =
    ScriptInterpreterError InterpreterError
  | ScriptPathViolation FilePath String
  | ScriptInvalidModuleName ModuleName
  | ScriptModuleNotFound ModuleName [FilePath]
  deriving (Generic, Show)

instance Exception ScriptError

data ScriptEnv = ScriptEnv {
  envModules :: !(Seq ModuleName)
, envTopModules :: !(Seq ModuleName)
} deriving (Generic)

newScriptEnv :: ScriptEnv
newScriptEnv = ScriptEnv {
    envModules = mempty
  , envTopModules = mempty
  }

newtype ScriptT (m :: * -> *) a = ScriptT { unScriptT :: StateT ScriptEnv (InterpreterT m) a }
  deriving (Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadMask
    , MonadIO, MonadState ScriptEnv)

instance MonadTrans ScriptT where
  lift = ScriptT . lift . lift
  {-# INLINE lift #-}

instance (MonadIO m, MonadMask m) => MonadInterpreter (ScriptT m) where
  fromSession a = ScriptT . lift $ fromSession a
  modifySessionRef a b =  ScriptT . lift $ modifySessionRef a b
  runGhc a = ScriptT . lift $ runGhc a
  {-# INLINE fromSession #-}
  {-# INLINE modifySessionRef #-}
  {-# INLINE runGhc #-}

instance (MonadIO m, MonadMask m) => MonadScript (ScriptT m) where
  addScriptModules ms = modify' $ \s -> s { envModules = envModules s <> S.fromList (F.toList ms) }
  getScriptModules = F.toList <$> gets envModules
  addTopLevelModule ms = modify' $ \s -> s { envTopModules = envTopModules s <> S.fromList (F.toList ms) }
  getTopLevelModules = F.toList <$> gets envTopModules
  {-# INLINE addScriptModules #-}
  {-# INLINE getScriptModules #-}
  {-# INLINE addTopLevelModule #-}
  {-# INLINE getTopLevelModules #-}

runScriptT :: (MonadIO m, MonadMask m) => ScriptT m a -> m (Either ScriptError a)
runScriptT ma = fmap (first ScriptInterpreterError) . unsafeRunInterpreterWithArgs args . flip evalStateT newScriptEnv $ do
  lift $ set [installedModulesInScope := False]
  unScriptT ma
  where
    args = ["-O2"] --["-fobject-code", "-O2"] doesn't work
    -- TODO: GhcException "Cannot add module M555953106601254686712058 to context: not interpreted"

setExtensions :: MonadScript m
  => [Extension]
  -> m ()
setExtensions es = set [languageExtensions := es]

loadScriptPackage :: MonadScript m
  => FilePath -- ^ Path to package YAML description
  -> m ()
loadScriptPackage pkgConfPath = do
  pkgConf <- readPackageConfig pkgConfPath
  basePath <- liftIO $ takeDirectory <$> canonicalizePath pkgConfPath
  searchPaths <- traverse (prepareSourcePath basePath) $ pkgSource pkgConf
  mapM_ (guardWithin basePath) searchPaths
  let modules = unpack <$> pkgModules pkgConf
  addScriptModules =<< traverse (resolveModuleName searchPaths) modules
  whenJust (pkgMainModule pkgConf) $ \p -> addTopLevelModule [unpack p]

-- | Convert module name to relative path
moduleToPath :: ModuleName -> FilePath
moduleToPath = (++ ".hs") . fmap dot2slash
  where
    dot2slash '.' = '/'
    dot2slash c = c

-- | Check that module name is valid (not a path)
isValidModuleName :: ModuleName -> Bool
isValidModuleName = not . any (\c -> c == '/' || c == '\\')

-- | Find module file in given search paths and return absolute path to it
resolveModuleName :: forall m . (MonadThrow m, MonadIO m) => [FilePath] -> ModuleName -> m FilePath
resolveModuleName paths m
  | not (isValidModuleName m) = throwM $ ScriptInvalidModuleName m
  | otherwise = do
    res <- F.foldrM resolve Nothing paths
    maybe (throwM $ ScriptModuleNotFound m paths) pure res
  where
    modulePath = moduleToPath m
    resolve :: FilePath -> Maybe FilePath -> m (Maybe FilePath)
    resolve path Nothing = liftIO $ do
      p <- makeAbsolute $ path </> modulePath
      check <- doesFileExist p
      pure $ if check then Just p else Nothing
    resolve _ res = pure res

guardWithin :: (MonadThrow m, MonadIO m) => FilePath -> FilePath -> m ()
guardWithin pbase p = if isRelative (makeRelative pbase p)
  then pure ()
  else throwM $ ScriptPathViolation p $ "Is not relative to " ++ show pbase

prepareSourcePath :: MonadIO m => FilePath -> FilePath -> m FilePath
prepareSourcePath pbase p = liftIO $ canonicalizePath $ pbase </> p

execScript :: forall m a . (MonadScript m, Typeable a)
  => String
  -> m a
execScript estr = do
  loadModules =<< getScriptModules
  setTopLevelModules =<< getTopLevelModules
  interpret estr (as :: a)
