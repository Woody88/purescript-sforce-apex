module Test.Main where

import Prelude
import Data.Tuple
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Array as Array
import Data.Foldable 
import Data.Traversable (foldl, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Aff.Class (liftAff)
import Effect.Console (logShow, log)
import Language.Apex.Parser (parseCompilationUnit)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.SOQL as SOQL 
import Test.Utils (getApexFileNames, readApexFile)
import Text.Parsing.Parser (parseErrorMessage)

main :: Effect Unit
main = launchAff_ $ do
  filenames    <- getApexFileNames 
  fileContents <- traverse readApexFile filenames
  let files = Array.zip filenames fileContents
  runSpec [consoleReporter] do
    SOQL.spec
    describe "Parses Apex" do
      foldM (\a b -> pure a *> runTest b) unit files
  where 
    runTest (Tuple filename filecontent) = 
      it filename do 
        testCase filecontent true

testCase apexFile expected = do 
  case parseCompilationUnit apexFile of 
    Left e  -> fail $ parseErrorMessage e
    Right a -> shouldEqual expected true 
