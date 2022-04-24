module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DocTest.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Time/Relative.hs"
  ]

exts :: [String]
exts =
  [ "-XApplicativeDo",
    "-XDeriveAnyClass",
    "-XDeriveDataTypeable",
    "-XDeriveGeneric",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XImportQualifiedPost",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XTypeApplications"
  ]
