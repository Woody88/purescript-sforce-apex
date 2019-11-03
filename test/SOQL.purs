module Test.SOQL where 

import Prelude (Unit, ($), (==), discard, mempty, pure)
import Control.Lazy (fix)
import Data.List (List(..), (:), singleton)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (supervise)
import Language.SOQL.Parser 
import Language.SOQL.Syntax
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Text.Parsing.Parser (ParseError)

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

    it "SetExpr" do
        let x        = "BillingState IN ('California', 'New York')"
            expected = Right (SetExpr (Name "BillingState") IN (String "California" : String "New York" : Nil))
        parse x setExpr `shouldEqual` expected

    it "LogicalExpr" do
        let x        = "NOT Name = 'Salesforce'"
            expected = Right (LogicalExpr (FieldExpr (Name "Name") EQ (String "Salesforce")) NOT Nothing)
        parse x logicalExpr `shouldEqual` expected

    it "LogicalExpr Infix" do
        let x        = "Name = 'Salesforce' OR Name = 'SForce'"
            expected = Right (LogicalExpr (FieldExpr (Name "Name") EQ (String "Salesforce")) OR (Just (FieldExpr (Name "Name") EQ (String "SForce"))))
        parse x logicalExpr `shouldEqual` expected

    describe "SimpleExpr" do 
        it "SExpr" do 
            let x        = "BillingState IN ('California', 'New York')"
                expected = Right (SExpr (SetExpr (Name "BillingState") IN (String "California" : String "New York" : Nil)))
            parse x simpleExpr `shouldEqual` expected

        it "FldExpr" do 
            let x        = "CreatedDate > YESTERDAY"
                expected = Right (FldExpr (FieldExpr (Name "CreatedDate") GT (DateFormula YESTERDAY)))
            parse x simpleExpr `shouldEqual` expected

        it "CondExpr" do 
            let x        = "(Name = 'Salesforce' OR Name = 'SForce')"
                expected = Right (CondExpr (LogicExpr (LogicalExpr (FieldExpr (Name "Name") EQ (String "Salesforce")) OR (Just (FieldExpr (Name "Name") EQ (String "SForce"))))))
            parse x simpleExpr `shouldEqual` expected

    describe "ConditionExpr" do 
        it "LogicExpr" do
            let x        = "NOT Name = 'Salesforce'"
                expected = Right (LogicExpr (LogicalExpr (FieldExpr (Name "Name") EQ (String "Salesforce")) NOT Nothing))
            parse x condExpr `shouldEqual` expected
    
    describe "Query Compilation" do 
        it "Simple Query" do 
            let x = "SELECT Account FROM Lead"
                select = singleton $ Name "Account"
                from   = singleton $ Name "Lead" 
                using = Nothing 
                orderBy = Nothing 
                limit = Nothing 
                offset = Nothing 
                expected = Right {select, from, "where": Nothing, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected


        it "Query with where clause" do 
            let x = "SELECT Account, RecordType FROM Lead WHERE Id = 'a9p000041321ACM'"
                select = (Name "Account" : Name "RecordType" : mempty)
                from   = pure $ Name "Lead" 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Name "Id") EQ (String "a9p000041321ACM")))))
                using = Nothing 
                orderBy = Nothing 
                limit = Nothing 
                offset = Nothing 
                expected = Right {select, from, "where": where_, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected

        
        it "Query with where and using clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM'"
                select = (Name "Account" : Name "RecordType" : mempty)
                from   = singleton $ Name "Lead" 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Name "Id") EQ (String "a9p000041321ACM")))))
                using  = Just Mine
                orderBy = Nothing 
                limit = Nothing 
                offset = Nothing 
                expected = Right {select, from, "where": where_, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected


        it "Query with where, using, and order by clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last"
                select = (Name "Account" : Name "RecordType" : mempty)
                from   = singleton $ Name "Lead" 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Name "Id") EQ (String "a9p000041321ACM")))))
                using  = Just Mine
                orderBy = Just (OrderByExpr (singleton (Name "Account")) ASC Last)
                limit = Nothing 
                offset = Nothing 
                expected = Right {select, from, "where": where_, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected

        it "Query with where, using, order by, and limit clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last Limit 10"
                select = (Name "Account" : Name "RecordType" : mempty)
                from   = singleton $ Name "Lead" 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Name "Id") EQ (String "a9p000041321ACM")))))
                using  = Just Mine
                orderBy = Just (OrderByExpr (singleton (Name "Account")) ASC Last)
                limit = Just (Integer 10) 
                offset = Nothing
                expected = Right {select, from, "where": where_, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected


        it "Query with where, using, order by, limit, and offset clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last Limit 10 OFFSET 10"
                select = (Name "Account" : Name "RecordType" : mempty)
                from   = singleton $ Name "Lead" 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Name "Id") EQ (String "a9p000041321ACM")))))
                using  = Just Mine
                orderBy = Just (OrderByExpr (singleton (Name "Account")) ASC Last)
                limit = Just (Integer 10) 
                offset = Just (Integer 10)
                expected = Right {select, from, "where": where_, using, orderBy, limit, offset}
            parse x queryCompilation `shouldEqual` expected
