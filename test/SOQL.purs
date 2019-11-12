module Test.SOQL where 

import Prelude (Unit, ($), (==), (<<<), discard, mempty, pure, map)
import Control.Lazy (fix)
import Data.List (List(..), (:), singleton, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (supervise)
import Language.SOQL.Parser 
import Language.SOQL.Syntax
import Language.SOQL.Syntax.Types
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

    it "Field" do
        let x        = "Lead.Contact.Phone"
            expected = Right (fieldT "Lead.Contact.Phone")
        parse x field `shouldEqual` expected

    it "functionSpec" do
        let x        = "FORMAT(convertCurrency(amount))"
            expected = Right (Tuple (MiscF FORMAT) (singleton $ FuncP (Tuple (MiscF CONVERT_CURRENCY) (singleton (FieldP $ fieldT "amount")))))
        parse x functionExpr `shouldEqual` expected

    it "FieldExpr" do
        let x        = "CreatedDate > YESTERDAY"
            expected = Right (FieldExpr (Field $ fieldT "CreatedDate") GT (DateFormula YESTERDAY))
        parse x fieldExpr `shouldEqual` expected

    it "SetExpr" do
        let x        = "BillingState IN ('California', 'New York')"
            expected = Right (SetExpr (Field $ fieldT "BillingState") IN (String "California" : String "New York" : mempty))
        parse x setExpr `shouldEqual` expected

    it "LogicalExpr" do
        let x        = "NOT Name = 'Salesforce'"
            expected = Right (LogicalExpr (FieldExpr (Field $ fieldT "Name") EQ (String "Salesforce")) NOT Nothing)
        parse x logicalExpr `shouldEqual` expected

    it "LogicalExpr Infix" do
        let x        = "Name = 'Salesforce' OR Name = 'SForce'"
            expected = Right (LogicalExpr (FieldExpr (Field $ fieldT "Name") EQ (String "Salesforce")) OR (Just (FieldExpr (Field $ fieldT "Name") EQ (String "SForce"))))
        parse x logicalExpr `shouldEqual` expected

    describe "SimpleExpr" do 
        it "SExpr" do 
            let x        = "BillingState IN ('California', 'New York')"
                expected = Right (SExpr (SetExpr (Field $ fieldT "BillingState") IN (String "California" : String "New York" : mempty)))
            parse x simpleExpr `shouldEqual` expected

        it "FldExpr" do 
            let x        = "CreatedDate > YESTERDAY"
                expected = Right (FldExpr (FieldExpr (Field $ fieldT "CreatedDate") GT (DateFormula YESTERDAY)))
            parse x simpleExpr `shouldEqual` expected

        it "CondExpr" do 
            let x        = "(Name = 'Salesforce' OR Name = 'SForce')"
                expected = Right (CondExpr (LogicExpr (LogicalExpr (FieldExpr (Field $ fieldT "Name") EQ (String "Salesforce")) OR (Just (FieldExpr (Field $ fieldT "Name") EQ (String "SForce"))))))
            parse x simpleExpr `shouldEqual` expected

    describe "ConditionExpr" do 
        it "LogicExpr" do
            let x        = "NOT Name = 'Salesforce'"
                expected = Right (LogicExpr (LogicalExpr (FieldExpr (Field $ fieldT "Name") EQ (String "Salesforce")) NOT Nothing))
            parse x condExpr `shouldEqual` expected

        it "Select expr" do 
            let x        = "SELECT c.Account a"
                expected = Right (singleton $ Tuple (FOF $ Field $ fieldT "c.Account") (Just $ Name "a"))
            parse x selectExpr `shouldEqual` expected

    describe "Query Compilation" do 
        it "Simple Query" do 
            let x = "SELECT Account FROM Lead"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "Account") Nothing  
                from_   = singleton $ Tuple (fieldT "Lead") Nothing   
                expected = Right $ defaultQuery {select = select_, from = from_} 
            parse x queryCompilation `shouldEqual` expected

        it "Query with where clause" do 
            let x = "SELECT Account, RecordType FROM Lead WHERE Id = 'a9p000041321ACM'"
                select_ = Tuple (FOF $ Field $ fieldT "Account") Nothing  : Tuple (FOF $ Field $ fieldT "RecordType") Nothing : mempty 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Id") EQ (String "a9p000041321ACM")))))
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_}
            parse x queryCompilation `shouldEqual` expected
        
        it "Query with where and using clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM'"
                select_ = Tuple (FOF $ Field $ fieldT "Account") Nothing  : Tuple (FOF $ Field $ fieldT "RecordType") Nothing : mempty 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Id") EQ (String "a9p000041321ACM")))))
                using_  = Just Mine
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, using = using_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with where, using, and order by clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last"
                select_ = Tuple (FOF $ Field $ fieldT "Account") Nothing  : Tuple (FOF $ Field $ fieldT "RecordType") Nothing : mempty 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Id") EQ (String "a9p000041321ACM")))))
                using_  = Just Mine
                orderBy_ = Just (OrderByExpr (singleton $ Tuple (FieldO $ fieldT "Account") Nothing) (Just Asc) (Just Last))
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, using = using_, orderBy = orderBy_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with where, using, order by, and limit clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last Limit 10"
                select_ = Tuple (FOF $ Field $ fieldT "Account") Nothing  : Tuple (FOF $ Field $ fieldT "RecordType") Nothing : mempty 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Id") EQ (String "a9p000041321ACM")))))
                using_  = Just Mine
                orderBy_ = Just (OrderByExpr (singleton $ Tuple (FieldO $ fieldT "Account") Nothing) (Just Asc) (Just Last))
                limit_ = Just (Integer 10) 
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, using = using_, orderBy = orderBy_, limit = limit_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with where, using, order by, limit, and offset clause" do 
            let x = "SELECT Account, RecordType FROM Lead USING SCOPE Mine WHERE Id = 'a9p000041321ACM' Order By Account Asc Nulls Last Limit 10 OFFSET 10"
                select_ = Tuple (FOF $ Field $ fieldT "Account") Nothing  : Tuple (FOF $ Field $ fieldT "RecordType") Nothing : mempty 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Id") EQ (String "a9p000041321ACM")))))
                using_  = Just Mine
                orderBy_ = Just (OrderByExpr (singleton $ Tuple (FieldO $ fieldT "Account") Nothing) (Just Asc) (Just Last))
                limit_ = Just (Integer 10) 
                offset_ = Just (Integer 10)
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, using = using_, orderBy = orderBy_, limit = limit_, offset = offset_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with update clause" do 
            let x = "SELECT Title FROM FAQ__kav WHERE Keyword = 'Apex' UPDATE TRACKING"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "Title") Nothing  
                from_   = singleton $ Tuple (fieldT "FAQ__kav") Nothing 
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "Keyword") EQ (String "Apex")))))
                update_ = Just $ singleton Tracking 
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, update = update_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with for clause" do 
            let x = "SELECT Id FROM Account LIMIT 2 FOR UPDATE"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "Id") Nothing 
                from_   = singleton $ Tuple (fieldT "Account") Nothing 
                limit_ = Just (Integer 2) 
                for_ = Just $ singleton Update
                expected = Right $ defaultQuery {select = select_, from = from_, limit = limit_, for = for_}
            parse x queryCompilation `shouldEqual` expected

        it "Query with clause" do 
            let x = "SELECT Title FROM Question WHERE LastReplyDate < 2005-10-08T01:02:03Z WITH DATA CATEGORY Product__c AT mobile_phones__c AND Product__c BELOW All__c"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "Title") Nothing
                from_   = singleton $ Tuple (fieldT "Question") Nothing  
                where_ = (Just (SimplExpr (FldExpr (FieldExpr (Field $ fieldT "LastReplyDate") LT (Datetime "2005-10-08T01:02:03Z")))))
                with_ = Just (FilterExpr (DataCategorySelection (fieldT "Product__c") At (fieldT "mobile_phones__c")) (Just (Tuple AND (FilterExpr (DataCategorySelection (fieldT "Product__c") Below (fieldT "All__c")) Nothing))))
                expected = Right $ defaultQuery {select = select_, from = from_, "where" = where_, with = with_}
            parse x queryCompilation `shouldEqual` expected

        it "Query typeof clause" do 
            let x = """SELECT 
                        TYPEOF What
                            WHEN Account THEN Phone, NumberOfEmployees
                            WHEN Opportunity THEN Amount, CloseDate
                            ELSE Name, Email
                        END
                        FROM Event"""
                select_ = singleton $ Tuple (TypeOf $ typeof) Nothing
                typeof  = TypeofExpr 
                            (Name "What") 
                            (Tuple (Name "Account") (fieldListT "Phone,NumberOfEmployees") : Tuple (Name "Opportunity") (fieldListT "Amount,CloseDate") : mempty) 
                            (fieldListT "Name,Email")
                from_   = singleton $ Tuple (fieldT "Event") Nothing  
                expected = Right $ defaultQuery {select = select_, from = from_}            
            parse x queryCompilation `shouldEqual` expected

        it "Query group by clause" do 
            let x = "SELECT LeadSource FROM Lead GROUP BY LeadSource"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "LeadSource") Nothing 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing 
                groupBy_ = Just $ FieldG $ fieldListT "LeadSource"
                expected = Right $ defaultQuery {select = select_, from = from_, groupBy = groupBy_}
            parse x queryCompilation `shouldEqual` expected

        it "Query group by rollup clause" do 
            let x = "SELECT LeadSource FROM Lead GROUP BY ROLLUP(LeadSource)"
                select_ = singleton $ Tuple (FOF $ Field $ fieldT "LeadSource") Nothing 
                from_   = singleton $ Tuple (fieldT "Lead") Nothing 
                groupBy_ = Just $ Rollup $ fieldListT "LeadSource"
                expected = Right $ defaultQuery {select = select_, from = from_, groupBy = groupBy_}
            parse x queryCompilation `shouldEqual` expected

        it "Query group by cube clause" do 
            let x = """SELECT Type, BillingCountry,
                            GROUPING(Type) grpType, GROUPING(BillingCountry) grpCty,
                            COUNT(id) accts
                        FROM Account
                        GROUP BY CUBE(Type, BillingCountry)
                        ORDER BY GROUPING(Type), GROUPING(BillingCountry)"""
                select_ = (Tuple (FOF $ Field $ fieldT "Type") Nothing : 
                           Tuple (FOF $ Field $ fieldT "BillingCountry") Nothing : 
                           Tuple (FOF $ Func $ Tuple (MiscF GROUPING) (singleton $ FieldP $ fieldT "Type")) (Just $ Name "grpType") :
                           Tuple (FOF $ Func $ Tuple (MiscF GROUPING) (singleton $ FieldP $ fieldT "BillingCountry")) (Just $ Name "grpCty") :
                           Tuple (FOF $ Func $ Tuple (AggrF COUNT) (singleton $ FieldP $ fieldT "id")) (Just $ Name "accts") :
                           mempty)
                from_   = singleton $ Tuple (fieldT "Account") Nothing 
                groupBy_ = Just $ Cube $ fieldListT "Type, BillingCountry"
                orderBy_ = Just (OrderByExpr 
                                    (Tuple (FuncO $ Tuple (MiscF GROUPING) (singleton $ FieldP $ fieldT "Type")) Nothing : 
                                     Tuple (FuncO $ Tuple (MiscF GROUPING) (singleton $ FieldP $ fieldT "BillingCountry")) Nothing :   
                                     mempty) 
                                    Nothing 
                                    Nothing)
                expected = Right $ defaultQuery {select = select_, from = from_, groupBy = groupBy_, orderBy = orderBy_}
            parse x queryCompilation `shouldEqual` expected

        it "Query having clause" do 
            let x = "SELECT LeadSource, COUNT(Name) FROM Lead GROUP BY LeadSource HAVING COUNT(Name) > 100"
                select_ = ( Tuple (FOF $ Field $ fieldT "LeadSource") Nothing :
                            Tuple (FOF $ Func $ Tuple (AggrF COUNT) (singleton $ FieldP $ fieldT "Name")) Nothing : 
                            mempty)
                from_   = singleton $ Tuple (fieldT "Lead") Nothing 
                having_ = (Just (SimplExpr (FldExpr (FieldExpr (Func $ Tuple (AggrF COUNT) (singleton $ FieldP $ fieldT "Name")) GT (Integer 100)))))
                groupBy_ = Just $ FieldG $ fieldListT "LeadSource"
                expected = Right $ defaultQuery {select = select_, from = from_, groupBy = groupBy_, having = having_}
            parse x queryCompilation `shouldEqual` expected
            
fieldT :: String -> Field
fieldT = map Name <<< fromFoldable <<< split (Pattern ".") <<< trim

fieldListT :: String -> List Field 
fieldListT = map fieldT <<< fromFoldable <<< split (Pattern ",") <<< trim

defaultQuery :: Query 
defaultQuery = 
    { select: mempty 
    , from: mempty 
    , "where": Nothing 
    , with: Nothing
    , using: Nothing 
    , orderBy: Nothing 
    , groupBy: Nothing
    , having: Nothing
    , limit: Nothing 
    , offset: Nothing
    , update: Nothing
    , for: Nothing
    }