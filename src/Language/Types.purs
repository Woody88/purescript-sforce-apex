module Language.Types where 

import Prelude
import Control.Lazy (fix)
import Data.List (List)
import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (Parser)

data L a = L Pos a

newtype Pos = Pos Position 

type P = Parser String 

data Token  = 
    -- Keywords
    KW_Abstract
    | KW_Assert | KW_Boolean | KW_Break | KW_Blob | KW_Case | KW_Catch | KW_Char | KW_Class | KW_Const
    | KW_Continue | KW_WhenElse | KW_Do | KW_Double | KW_Enum | KW_Extends | KW_Final | KW_Finally | KW_Decimal 
    | KW_For | KW_If | KW_Implements | KW_Import | KW_Instanceof | KW_Integer | KW_Interface | KW_Long 
    | KW_New | KW_Private | KW_Protected | KW_Public | KW_Return | KW_Short | KW_Static | KW_Super 
    | KW_Switch | KW_This | KW_Throw | KW_Throws | KW_Transient | KW_Try | KW_Void | KW_While 
    | KW_Object | KW_Time | KW_Date | KW_Datetime | KW_Override | KW_Virtual | KW_Global | KW_String
    | KW_As | KW_Asc | KW_OrderBy | KW_GroupByCube | KW_Desc | KW_Else | KW_NullFirst | KW_From | KW_GroupBy 
    | KW_Having | KW_NullLast | KW_Limit | KW_GroupByRollup | KW_Select | KW_Using | KW_When | KW_End
    | KW_Where | KW_Then 
    
    -- DML 
    | KW_Update | KW_Insert | KW_Upsert | KW_Delete | KW_Undelete | KW_Merge

    -- Sharing 
    | KW_With_Share | KW_Without_Share | KW_Inherit_Share

    -- Symbols
    | OpenParen | CloseParen | OpenSquare | CloseSquare | OpenCurly | CloseCurly | SemiColon | Comma | Period 
    | Colon 

    -- Literals 
    | IntegerTok Int | LongTok BigInt | DoubleTok Number | CharTok Char | StringTok String | BoolTok Boolean | KW_SOQL String      
    | DateTok String | DatetimeTok String | NullTok

    -- Identifiers
    | IdentTok String 

    -- Operators
    | OpTok String
    | Op_Equal | Op_GThan | Op_LThan | Op_Bang | Op_Tilde | Op_Query | Op_Colon | Op_Equals | Op_LThanE | Op_GThanE | Op_BangE | Op_AAnd 
    | Op_OOr | Op_PPlus | Op_MMinus | Op_Plus | Op_Minus | Op_Star | Op_Slash | Op_And | Op_Or | Op_Caret | Op_Percent | Op_LShift | Op_PlusE 
    | Op_MinusE | Op_StarE | Op_SlashE | Op_AndE | Op_OrE | Op_CaretE | Op_PercentE | Op_LShiftE | Op_RShiftE | Op_RRShiftE | Op_AtSign 
    
    -- SOQL extra omparison Operators
    | Op_NotEq | Op_In | Op_NotIn
    | Op_Not  | Op_Excludes | Op_Includes | Op_Like

    -- SOQL Date Functions
    | KW_Calendar_month | KW_Calendar_quarter | KW_Calendar_year | KW_Day_in_month 
    | KW_Day_in_week | KW_Day_in_year | KW_Day_only | KW_Fiscal_month | KW_Fiscal_quarter 
    | KW_Fiscal_year | KW_Hour_in_day | KW_Week_in_month | KW_Week_in_year

    -- SOQL Aggregate Functions
    | KW_Avg | KW_Count | KW_Count_distinct | KW_Min | KW_Max | KW_Sum

    -- SOQL Location Functions
    | KW_Distance | KW_Geolocation

    -- SOQL Other Functions
    | KW_Format | KW_Tolabel | KW_Convert_time_zone | KW_Convert_currency | KW_Grouping

    -- Date Literals
    | Yesterday | Today | Tomorrow | Last_week | This_week | Next_week | Last_month | This_month 
    | Next_month | Last_90_days | Next_90_days | This_quarter | Last_quarter | Next_quarter | This_year 
    | Last_year | Next_year | This_fiscal_quarter | Last_fiscal_quarter | Next_fiscal_quarter | This_fiscal_year 
    | Last_fiscal_year | Next_fiscal_year

    -- Date n Literals
    | Next_n_days Int | Last_n_days Int | N_days_ago Int | Next_n_weeks Int | Last_n_weeks Int | N_weeks_ago Int| Next_n_months Int
    | Last_n_months Int | N_months_ago Int | Next_n_quarters Int | Last_n_quarters Int | N_quarters_ago Int | Next_n_years Int 
    | Last_n_years Int | N_years_ago Int | Next_n_fiscal_quarters Int | Last_n_fiscal_quarters Int | N_fiscal_quarters_ago Int 
    | Next_n_fiscal_years Int | Last_n_fiscal_years Int | N_fiscal_years_ago Int

derive instance genericToken :: Generic Token _

derive instance eqToken :: Eq Token 


instance showToken :: Show Token where 
    show = genericShow 

derive instance newtypePos :: Newtype Pos _
derive instance genericL :: Generic (L a) _

derive instance eqPos :: Eq Pos 
derive instance eqL :: Eq a => Eq (L a)

instance showL :: Show a => Show (L a) where 
    show = genericShow

instance showPos :: Show Pos where 
    show (Pos (Position p)) = "(" <> show p.line <> "," <> show p.column <> ")"
