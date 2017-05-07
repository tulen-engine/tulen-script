module Game.Tulen.Script.Package(
    Version
  , ConstraintClause(..)
  , satisfyConstraint
  , parseConstraintClause
  , encodeConstraintClause
  , VersionConstraint
  , versionConstraint
  , constraintPackage
  , constraintClause
  , parseVersionConstraint
  , encodeVersionConstraint
  , PackageConfig
  , pkgName
  , pkgVersion
  , pkgSource
  , pkgMainModule
  , pkgModules
  , pkgDependencies
  , defaultPackageConfig
  , readPackageConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types hiding (Parser, parse)
import Data.Attoparsec.Text
import Data.Char
import Data.Monoid
import Data.SemVer
import Data.Text
import Data.Yaml.Config
import GHC.Generics

import qualified Data.Attoparsec.Text as AT

data ConstraintClause =
    VersionEqual Version
  | VersionGreater Version
  | VersionGreaterEqual Version
  | VersionLess Version
  | VersionLessEqual Version
  | VersionAnd ConstraintClause ConstraintClause
  deriving (Show, Eq, Generic)

satisfyConstraint :: Version -> ConstraintClause -> Bool
satisfyConstraint ver cc = case cc of
  VersionEqual v -> ver == v
  VersionGreater v -> ver > v
  VersionGreaterEqual v -> ver >= v
  VersionLess v -> ver < v
  VersionLessEqual v -> ver <= v
  VersionAnd c1 c2 -> satisfyConstraint ver c1 && satisfyConstraint ver c2

parseVersion :: Parser Version
parseVersion = do
  t <- takeTill isSpace
  either fail pure $ fromText t

constraintClauseParser :: Parser ConstraintClause
constraintClauseParser = andCase <|> basicCase
  where
  basicCase = do
    skipSpace
    tag <- AT.takeWhile (inClass "=><")
    skipSpace
    c1 <- case tag of
      "==" -> VersionEqual <$> parseVersion <?> "equal"
      ">"  -> VersionGreater <$> parseVersion <?> "greater"
      ">=" -> VersionGreaterEqual <$> parseVersion <?> "greater equal"
      "<"  -> VersionLess <$> parseVersion <?> "less"
      "<=" -> VersionLessEqual <$> parseVersion <?> "less equal"
      _    -> fail $ "Unexpected " ++ unpack tag
    pure c1
  andCase = do
    c1 <- basicCase
    skipSpace
    _ <- asciiCI "&&"
    c2 <- constraintClauseParser
    pure $ VersionAnd c1 c2

parseConstraintClause :: Text -> Either String ConstraintClause
parseConstraintClause = parseOnly constraintClauseParser

encodeConstraintClause :: ConstraintClause -> Text
encodeConstraintClause cc = case cc of
  VersionEqual v -> "== " <> toText v
  VersionGreater v -> "> " <> toText v
  VersionGreaterEqual v -> ">= " <> toText v
  VersionLess v -> "< " <> toText v
  VersionLessEqual v -> "<= " <> toText v
  VersionAnd c1 c2 -> encodeConstraintClause c1 <> " && " <> encodeConstraintClause c2

data VersionConstraint = VersionConstraint {
  constraintPackage   :: !Text
, constraintClause    :: !(Maybe ConstraintClause)
} deriving (Show, Eq, Generic)

versionConstraint :: Text -> Maybe ConstraintClause -> VersionConstraint
versionConstraint = VersionConstraint

parseVersionConstraint :: Text -> Either String VersionConstraint
parseVersionConstraint = parseOnly p
  where
    p = do
      skipSpace
      name <- takeTill isSpace
      skipSpace
      VersionConstraint name <$> optional constraintClauseParser

encodeVersionConstraint :: VersionConstraint -> Text
encodeVersionConstraint VersionConstraint{..} = constraintPackage <> maybe "" ((" " <>) . encodeConstraintClause) constraintClause

instance FromJSON VersionConstraint where
  parseJSON (String s) = either (fail . ("Failed to parse version constraint: " ++)) pure $ parseVersionConstraint s
  parseJSON wut = typeMismatch "VersionConstraint" wut

instance ToJSON VersionConstraint where
  toJSON = String . encodeVersionConstraint

data PackageConfig = PackageConfig {
  pkgName         :: !Text
, pkgVersion      :: !Version
, pkgSource       :: ![FilePath]
, pkgMainModule   :: !(Maybe Text)
, pkgModules      :: ![Text]
, pkgDependencies :: ![VersionConstraint]
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

instance FromJSON Version where
  parseJSON (String s) = either (fail . ("Failed to parse version: " ++)) pure . fromText $ s
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
