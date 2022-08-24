{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Runner (runTests) where

import System.Environment
import Test.Hspec
import Test.Hspec.Runner

#if MIN_VERSION_hspec_junit_formatter(1,0,2)
import Test.Hspec.JUnit
import Data.String
#else
import Test.HSpec.JUnit
#endif

runTests :: String -> Spec -> IO ()
runTests name spec = do
  junitEnabled <- lookupEnv "JUNIT_ENABLED"
  junitOutputDirectory <- lookupEnv "JUNIT_OUTPUT_DIRECTORY"
  case (junitEnabled, junitOutputDirectory) of
    (Just "1", Just path) -> do
#if MIN_VERSION_hspec_junit_formatter(1,0,2)
      let
        junitConfig = setJUnitConfigOutputName "test_results.xml" $
          setJUnitConfigOutputDirectory (path <> "/" <> name) $ defaultJUnitConfig (fromString name)
        hspecConfig = configWithJUnit junitConfig defaultConfig

      summary@(Summary e f) <- hspecWithResult hspecConfig spec
      _ <- putStrLn $ "Total " ++ show e ++ " examples, failed " ++ show f ++ " examples."
      evaluateSummary summary
#else
      summary@(Summary e f) <- runJUnitSpec spec (path, name) defaultConfig
      _ <- putStrLn $ "Total " ++ show e ++ " examples, failed " ++ show f ++ " examples."
      evaluateSummary summary
#endif
    _ -> hspec spec