module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (foldl, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Console (logShow, log)
import Language.Apex.Parser (parseCompilationUnit)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utils (getApexFileNames, readApexFile)
import Text.Parsing.Parser (parseErrorMessage)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-spec" do
    describe "Attributes" do
      foldl (*>) (shouldEqual true true)  [shouldEqual true true]


-- test2 b a =
--     it ("1." <> show a) do
--       shouldEqual true a

        -- it "awesome" do
      --   let isAwesome = true
      --   -- isAwesome `shouldEqual` true
      --   traverse (shouldEqual true) [true, true, true]

  -- describe "Apex File" do
  --   it "all apex files" do
  --     liftEffect $ log "Hello"
  --     files <- getApexFileNames >>= traverse readApexFile
  --     liftEffect (log $ "[DEBUG]" <> show files)


-- testCase apexFile = do 
--   case parseCompilationUnit apexFile of 
--     Left e  -> fail $ parseErrorMessage e
--     Right a -> shouldEqual true true 
