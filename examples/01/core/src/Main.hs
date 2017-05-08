module Main where

import API
import Game.Tulen.Script
import Data.Text (unpack)

main :: IO ()
main = do
  res <- runScriptT $ do
    setExtensions [
        NoImplicitPrelude
      , RecordWildCards
      , Safe
      ]
    loadScriptPackage "api/plugin.yaml"
    loadScriptPackage "plugin/plugin.yaml"
    execScript "plugin"
  case res of
    Left e -> putStrLn $ "Error: " ++ unpack (prettyScriptError e)
    Right OutputAPI{..} -> do
      let inputApi = InputAPI (pure 42)
          run = flip runRIO inputApi
      putStrLn "Executing script:"
      putStr "func1 10 42 = "
      print =<< run (func1 10 42)
      putStr "func2 10 42 = "
      print =<< run (func2 10 42)
