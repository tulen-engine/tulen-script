module Game.Tulen.Script.PackageSpec where

import Data.SemVer
import Test.Hspec
import Test.HUnit

import Game.Tulen.Script.Package

assertRight :: (Eq a, Show a) => a -> Either String a -> IO ()
assertRight a (Left s) = assertFailure $ "Failed, expected " ++ show a ++ ", got: " ++ s
assertRight a (Right a') = assertEqual "Right value" a a'

spec :: Spec
spec = do
  it "Parses version constraints" $ do
    assertRight (VersionEqual (version 0 1 0 [] [])) $ parseConstraintClause "== 0.1.0"
    assertRight (VersionEqual (version 0 1 1 [] [])) $ parseConstraintClause "==0.1.1"
    assertRight (VersionEqual (version 0 1 2 [] [])) $ parseConstraintClause " == 0.1.2"
    assertRight (VersionEqual (version 0 1 3 [] [])) $ parseConstraintClause "== 0.1.3 "
    assertRight (VersionGreater (version 0 1 0 [] [])) $ parseConstraintClause "> 0.1.0"
    assertRight (VersionGreaterEqual (version 0 1 0 [] [])) $ parseConstraintClause ">= 0.1.0"
    assertRight (VersionLess (version 0 1 0 [] [])) $ parseConstraintClause "< 0.1.0"
    assertRight (VersionLessEqual (version 0 1 0 [] [])) $ parseConstraintClause "<= 0.1.0"
    let v1 = version 0 1 0 [] []
        v2 = version 0 2 0 [] []
    assertRight (VersionAnd (VersionGreaterEqual v1) (VersionLess v2)) $ parseConstraintClause ">= 0.1.0 && < 0.2.0"
    assertRight (VersionAnd (VersionGreaterEqual v1) (VersionLess v2)) $ parseConstraintClause ">= 0.1.0 && < 0.2.0 "
    assertRight (VersionAnd (VersionGreaterEqual v1) (VersionLess v2)) $ parseConstraintClause " >= 0.1.0 && < 0.2.0"
  it "Parses version constraint with package" $ do
    assertRight (versionConstraint "api" $ Just $ VersionEqual (version 0 1 0 [] [])) $ parseVersionConstraint "api == 0.1.0"
    assertRight (versionConstraint "api" Nothing) $ parseVersionConstraint "api"
