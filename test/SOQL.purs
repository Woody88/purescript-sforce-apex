module Test.SOQL where 

import Prelude (Unit, discard, mempty)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Effect (Effect)
import Language.SOQL.Parser 
import Language.SOQL.Syntax
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "SOQL Parser case test parses" do
    it "Name" do
        let x        = "CreatedDate"
            expected = Right (Name "CreatedDate") 
        parse x name `shouldEqual` expected

    it "Name Ref" do
        let x        = "Lead.Contact.Phone"
            expected = Right (Ref (Name "Lead" : Name "Contact" : Name "Phone" : Nil))
        parse x name `shouldEqual` expected


    it "FieldExpr" do
        let x        = "CreatedDate > YESTERDAY"
            expected = Right (FieldExpr (Name "CreatedDate") GT (DateFormula YESTERDAY))
        parse x fieldExpr `shouldEqual` expected
