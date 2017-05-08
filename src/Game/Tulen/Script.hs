module Game.Tulen.Script(
  -- * Monad
    ScriptT
  , runScriptT
  , ScriptError(..)
  , InterpreterError(..)
  , GhcError(..)
  , prettyScriptError
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
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Sequence (Seq)
import Data.Text (Text, unpack, pack)
import Data.Typeable
import GHC.Generics
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory
import System.FilePath

import Game.Tulen.Script.Package

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

class MonadInterpreter m => MonadScript m where
  addScriptModules :: Foldable f => f ModuleName -> m ()
  getScriptModules :: m [ModuleName]

  addTopLevelModule :: Foldable f => f ModuleName -> m ()
  getTopLevelModules :: m [ModuleName]

  addScriptPackage :: Text -> Version -> m ()
  lookupScriptPackage :: Text -> m (Maybe Version)

data ScriptError =
    ScriptInterpreterError InterpreterError
  | ScriptPathViolation FilePath String
  | ScriptInvalidModuleName ModuleName
  | ScriptModuleNotFound ModuleName [FilePath]
  | ScriptPackageConflict PackageName Version Version
  | ScriptMissingDependency PackageName VersionConstraint
  | ScriptInvalidDependency PackageName VersionConstraint Version
  deriving (Generic, Show)

instance Exception ScriptError

prettyScriptError :: ScriptError -> Text
prettyScriptError e = case e of
  ScriptInterpreterError ie -> case ie of
    UnknownError emsg -> "GHC unknown error: " <> pack emsg
    WontCompile es -> "GHC compilation errors: " <> pack (unlines (errMsg <$> es))
    NotAllowed emsg -> "GHC not allow: " <> pack emsg
    GhcException emsg -> "GHC exception: " <> pack emsg
  ScriptPathViolation p emsg -> "Path '" <> pack p <> "' is invalid. " <> pack emsg
  ScriptInvalidModuleName name -> "Module name '" <> pack name <> "' is invalid."
  ScriptModuleNotFound name src -> "Cannot find module '" <> pack name <> "'. Searched in:\n"
    <> pack (unlines (("\t- " <>) <$> src))
  ScriptPackageConflict name installedVer ver -> "Package " <> name <> "-" <> encodeVersion ver
    <> " is already loaded with version " <> pack (show installedVer)
  ScriptMissingDependency requesterName constr -> "Package " <> requesterName <> " require package "
    <> encodeVersionConstraint constr
  ScriptInvalidDependency requesterName constr installedVer -> "Package " <> requesterName <> " requires "
    <> encodeVersionConstraint constr <> ", but installed version is " <> encodeVersion installedVer

data ScriptEnv = ScriptEnv {
  envModules :: !(Seq ModuleName)
, envTopModules :: !(Seq ModuleName)
, envPackages :: !(Map Text Version)
} deriving (Generic)

newScriptEnv :: ScriptEnv
newScriptEnv = ScriptEnv {
    envModules = mempty
  , envTopModules = mempty
  , envPackages = mempty
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
  addScriptPackage name version = do
    mver <- lookupScriptPackage name
    whenJust mver $ \installedVersion -> throwM $ ScriptPackageConflict name installedVersion version
    modify' $ \s -> s { envPackages = M.insert name version $ envPackages s }
  lookupScriptPackage name = M.lookup name <$> gets envPackages
  {-# INLINE addScriptModules #-}
  {-# INLINE getScriptModules #-}
  {-# INLINE addTopLevelModule #-}
  {-# INLINE getTopLevelModules #-}
  {-# INLINE addScriptPackage #-}
  {-# INLINE lookupScriptPackage #-}

runScriptT :: (MonadIO m, MonadMask m) => ScriptT m a -> m (Either ScriptError a)
runScriptT ma = wrapExceptions . fmap (first ScriptInterpreterError) . unsafeRunInterpreterWithArgs args . flip evalStateT newScriptEnv $ do
  lift $ set [installedModulesInScope := False]
  unScriptT ma
  where
    wrapExceptions ma = ma `catch` (pure . Left)
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
  mapM_ (guardPackageDep $ pkgName pkgConf) $ pkgDependencies pkgConf
  basePath <- liftIO $ takeDirectory <$> canonicalizePath pkgConfPath
  searchPaths <- traverse (prepareSourcePath basePath) $ pkgSource pkgConf
  mapM_ (guardWithin basePath) searchPaths
  let modules = unpack <$> pkgModules pkgConf
  addScriptModules =<< traverse (resolveModuleName searchPaths) modules
  whenJust (pkgMainModule pkgConf) $ \p -> addTopLevelModule [unpack p]
  addScriptPackage (pkgName pkgConf) (pkgVersion pkgConf)

-- | Check that dependency is in place and satisfy constraints
guardPackageDep :: MonadScript m => PackageName -> VersionConstraint -> m ()
guardPackageDep name constr = do
  mver <- lookupScriptPackage (constraintPackage constr)
  case mver of
    Nothing -> throwM $ ScriptMissingDependency name constr
    Just ver -> case constraintClause constr of
      Nothing -> pure ()
      Just clause -> unless (satisfyConstraint ver clause) $ throwM $ ScriptInvalidDependency name constr ver

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
