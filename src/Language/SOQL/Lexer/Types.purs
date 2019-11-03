module Language.SOQL.Lexer.Types where 

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser (Parser)

type P = Parser String

data Token = 
    -- Reserved Keywords 
    KW_As | KW_Asc | KW_OrderBy | KW_Cube | KW_Desc | KW_Else 
    | KW_NullFirst | KW_From | KW_Group | KW_Having  | KW_NullLast  
    | KW_Limit | KW_Rollup | KW_Select | KW_Using 
    | KW_Where | KW_With | KW_For | KW_Update 

    -- Literals 
    | IntegerTok Int | LongTok BigInt | DoubleTok Number | StringTok String | BoolTok Boolean 
    | DateTok String | DatetimeTok String | NullTok 

    -- Non Reserved Keyword 
    | Above | Above_or_below | At | Below | Category | Data | End | Group
    | Order | Reference | Tracking | Then | Typeof | View | Viewstat | When

    -- Comparison Operators
    | Op_Eq | Op_GThan | Op_LThan | Op_LThanE | Op_GThanE | Op_NotEq | Op_In | Op_NotIn
    | Op_Not  | Op_Or | Op_Excludes | Op_Includes | Op_And | Op_Like

    -- Date Functions
    | Calendar_month | Calendar_quarter | Calendar_year | Day_in_month 
    | Day_in_week | Day_in_year | Day_only | Fiscal_month | Fiscal_quarter 
    | Fiscal_year | Hour_in_day | Week_in_month | Week_in_year

    -- Aggregate Functions
    | Avg | Count | Count_distinct | Min | Max | Sum

    -- Location Functions
    | Distance | Geolocation

    --Other Functions
    | Format | Tolabel | Convert_time_zone | Convert_currency | Grouping

    -- Symbols
    | Period | Colon | SemiColon | Comma | Asterisk | OpenParen | CloseParen | Plus | Minus 
    | CloseSquare | OpenSquare | Slash

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

    -- Identifiers
    | Ident String 

derive instance genericToken :: Generic Token _ 

derive instance eqToken :: Eq Token 

instance showToken :: Show Token where
    show = genericShow