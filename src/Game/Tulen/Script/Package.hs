module Game.Tulen.Script.Package(
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
import Data.SemVer
import Data.Text
import Data.Yaml.Config
import GHC.Generics

data PackageConfig = PackageConfig {
  pkgName         :: !Text
, pkgVersion      :: !Version
, pkgSource       :: ![FilePath]
, pkgMainModule   :: !(Maybe Text)
, pkgModules      :: ![Text]
, pkgDependencies :: ![Text] -- TODO
} deriving (Show, Eq, Generic)

defaultPackageConfig :: PackageConfig
defaultPackageConfig = PackageConfig {
    pkgName = ""
  , pkgVersion = initial
  , pkgSource = []
  , pkgMainModule = Nothing
  , pkgModules = []
  , pkgDependencies = []
  }

-- | Parse semversion from JSON
parseVersion :: Text -> Parser Version
parseVersion = either (fail . ("Failed to parse version: " ++)) pure . fromText

instance FromJSON Version where
  parseJSON (String s) = parseVersion s
  parseJSON wut = typeMismatch "Version" wut

instance ToJSON Version where
  toJSON = String . toText

instance FromJSON PackageConfig where
  parseJSON (Object o) = PackageConfig
    <$> o .: "name"
    <*> o .:? "version" .!= initial
    <*> o .:? "source" .!= []
    <*> o .:? "main-module"
    <*> o .:? "modules" .!= []
    <*> o .:? "dependencies" .!= []
  parseJSON wat = typeMismatch "PackageConfig" wat

instance ToJSON PackageConfig where
  toJSON PackageConfig{..} = object [
      "name"         .= pkgName
    , "version"      .= toText pkgVersion
    , "source"       .= pkgSource
    , "main-module"  .= pkgMainModule
    , "modules"      .= pkgModules
    , "dependencies" .= pkgDependencies
    ]

readPackageConfig :: MonadIO m => FilePath -> m PackageConfig
readPackageConfig path = liftIO $ loadYamlSettings [path] [] ignoreEnv
