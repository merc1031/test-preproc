{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.FilePath.Find (find, always, fileName)
import System.FilePath.Posix (dropExtension)
import Control.Monad (liftM)
import Data.List (isSuffixOf, stripPrefix, intercalate)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc, qq)
import Control.Applicative ((<$>), liftA2)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Safe

import Debug.Trace

replace old new = intercalate new . splitOn old

doMain :: [String] -> IO ()
doMain args = do
    let orig = args !! 0
        input = args !! 1
        out = args !! 2
        root = fromMaybe "tests" $ atMay args 3
        tests = findTests root
        specs = findSpecs root
    content <- liftA2 (testFile root) tests specs
    writeFile "/tmp/test-preproc" content
    writeFile out content

findTests = findB "Test.hs"
findSpecs = findB "Spec.hs"

findB suff root = find always ((isSuffixOf suff) `liftM` fileName) root

dropDir :: String -> String -> Maybe String
dropDir root = stripPrefix root'
  where
    root' = root <> "/"

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

importTests :: String -> [String] -> [String]
importTests root = mapMaybe ((fmap $ putTogether . noSlash . noExts) . dropDir root)

importTestsS :: String -> [String] -> String
importTestsS root = unlines . importTests root

testsSection :: String -> String -> (String -> String) -> [String] -> [String]
testsSection root mainIs xform = mapMaybe ((fmap $ createTest mainIs xform . noSlash . noExts) . dropDir root)

testsSectionS :: String -> String -> [String] -> String
testsSectionS root mainIs = testsSectionS' root mainIs id

testsSectionS' :: String -> String -> (String -> String) -> [String] -> String
testsSectionS' root mainIs xform = intercalate "\n    , " . testsSection root mainIs xform

testFile :: String -> [String] -> [String] -> String
testFile root tests specs = [qq|
module Main where

import Test.Tasty as Tasty
import Test.Tasty.Hspec as HSpec

{ importTestsS root tests }
{ importTestsS root specs }

silenceRedundantImportWarnings :: Spec -> IO [TestTree]
silenceRedundantImportWarnings = HSpec.testSpecs

tastyTestGroup :: TestTree
tastyTestGroup = let
  tastyTests =
    [
      { testsSectionS root ".tests" tests }
    ]
  in testGroup "tasty tests" tastyTests

specTests :: [IO TestTree]
specTests =
    [
      { testsSectionS' root ".spec" specToTasty specs }
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
