{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.FilePath.Find (find, always, fileName)
import System.FilePath.Posix (dropExtension)
import Control.Monad (liftM)
import Data.List (isSuffixOf, stripPrefix, intercalate)
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc, qq)
import Control.Applicative ((<$>), liftA2)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import Debug.Trace

replace old new = intercalate new . splitOn old

doMain :: [String] -> IO ()
doMain args = do
    let orig = args !! 0
        input = args !! 1
        out = args !! 2
        tests = findTests
        specs = findSpecs
    content <- liftA2 testFile tests specs
    writeFile "/tmp/test-preproc" content
    writeFile out content

findTests = findB "Test.hs"
findSpecs = findB "Spec.hs"

findB suff = find always ((isSuffixOf suff) `liftM` fileName) "tests"

dropDir :: String -> Maybe String
dropDir = stripPrefix "tests/"

noExts :: String -> String
noExts = dropExtension

noSlash :: String -> String
noSlash = replace "/" "."

putTogether :: String -> String
putTogether = ("import qualified " ++)

createTest :: String -> (String -> String) -> String -> String
createTest mainIs xform = xform . ((flip (++)) mainIs)

specToTasty :: String -> String
specToTasty testRow = "( " ++ "HSpec.testSpec " ++ name ++ " " ++ testRow ++ " )"
  where
    name = "\"" ++ noExts testRow ++ "\""

importTests :: [String] -> [String]
importTests = mapMaybe ((fmap $ putTogether . noSlash . noExts) . dropDir)

importTestsS :: [String] -> String
importTestsS = unlines . importTests

testsSection :: String -> (String -> String) -> [String] -> [String]
testsSection mainIs xform = mapMaybe ((fmap $ createTest mainIs xform . noSlash . noExts) . dropDir)

testsSectionS :: String -> [String] -> String
testsSectionS mainIs = testsSectionS' mainIs id

testsSectionS' :: String -> (String -> String) -> [String] -> String
testsSectionS' mainIs xform = intercalate "\n  , " . testsSection mainIs xform

testFile :: [String] -> [String] -> String
testFile tests specs = [qq|
module Main where

import Test.Tasty as Tasty
import Test.Tasty.Hspec as HSpec

{ importTestsS tests }
{ importTestsS specs }

tastyTestGroup :: TestTree
tastyTestGroup = testGroup "tasty tests" tastyTests
  where
    tastyTests =
      [
        { testsSectionS ".tests" tests }
      ]

specTests :: [IO TestTree]
specTests =
  [
    { testsSectionS' ".spec" specToTasty specs }
  ]

main
  :: IO ()
main = do
  specTrees <- sequence specTests
  let
    specTestGroup = testGroup "spec tests" specTrees
  Tasty.defaultMain $ testGroup "all tests" [tastyTestGroup, specTestGroup]
|]

main = do
    putStrLn $ "Starting preprocessor"
    args <- getArgs
    doMain args
