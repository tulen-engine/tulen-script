module Game.Crafty.Script.Package(
    PackageConfig
  , pkgName
  , pkgVersion
  , pkgSource
  , pkgMainModule
  , pkgModules
  , pkgDependencies
  , defaultPackageConfig
  , readPackageConfig
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Data
import Data.Text
import Data.Yaml.Config
import GHC.Generics

data PackageConfig = PackageConfig {
  pkgName         :: !Text
, pkgVersion      :: !Text -- TODO
, pkgSource       :: ![FilePath]
, pkgMainModule   :: !(Maybe Text)
, pkgModules      :: ![Text]
, pkgDependencies :: ![Text] -- TODO
} deriving (Show, Eq, Generic, Data)

defaultPackageConfig :: PackageConfig
defaultPackageConfig = PackageConfig {
    pkgName = ""
  , pkgVersion = "0.0.0.0"
  , pkgSource = []
  , pkgMainModule = Nothing
  , pkgModules = []
  , pkgDependencies = []
  }

instance FromJSON PackageConfig where
  parseJSON (Object o) = PackageConfig
    <$> o .: "name"
    <*> o .:? "version" .!= "0.0.0.0"
    <*> o .:? "source" .!= []
    <*> o .:? "main-module"
    <*> o .:? "modules" .!= []
    <*> o .:? "dependencies" .!= []
  parseJSON wat = typeMismatch "PackageConfig" wat

instance ToJSON PackageConfig where
  toJSON PackageConfig{..} = object [
      "name"         .= pkgName
    , "version"      .= pkgVersion
    , "source"       .= pkgSource
    , "main-module"  .= pkgMainModule
    , "modules"      .= pkgModules
    , "dependencies" .= pkgDependencies
    ]

readPackageConfig :: MonadIO m => FilePath -> m PackageConfig
readPackageConfig path = liftIO $ loadYamlSettings [path] [] ignoreEnv
