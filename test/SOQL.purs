module Test.SOQL where 

import Prelude (Unit)
import Data.Either (Either(..))
import Effect (Effect)
import Language.SOQL.Parser 
import Language.SOQL.Syntax
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "SOQL Parser case test parses" do
    it "FieldExpr" do
        let x        = "CreatedDate > YESTERDAY"
            expected = Right (FieldExpr (Name "CreatedDate") GT (DateFormula YESTERDAY))
        parse x fieldExpr `shouldEqual` expected
