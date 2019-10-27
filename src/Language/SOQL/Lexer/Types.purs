module Language.SOQL.Lexer.Types where 

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

type P a = Parser String a

data Token = 
    -- Reserved Keywords 
    KW_And | KW_As | KW_Asc | KW_By | KW_Cube | KW_Desc | KW_Else | KW_Excludes | KW_False 
    | KW_First | KW_From | KW_Group | KW_Having | KW_In | KW_Includes | KW_Last | KW_Like 
    | KW_Limit | KW_Not | KW_Null | KW_Nulls | KW_Or | KW_Rollup | KW_Select | KW_True | KW_Using 
    | KW_Where | KW_With | KW_For | KW_Update

    -- Non Reserved Keyword 
    | Above | Above_or_below | At | Below | Category | Data | End | Offset | Group
    | Order | Reference | Scope | Tracking | Then | Typeof | View | Viewstat | When

    -- Comparison Operators
    | Op_Equals | Op_BangE | Op_GThan | Op_LThan | Op_LThanE | Op_GThanE | Op_Like 
    | Op_In | Op_NotIn | Op_Excludes | Op_Includes

    -- Logical Operators
    | Op_And | Op_Or | Op_Not 

    -- Date Functions
    | FN_Calendar_month | FN_Calendar_quarter | FN_Calendar_year | FN_Day_in_month 
    | FN_Day_in_week | FN_Day_in_year | FN_Day_only | FN_Fiscal_month | FN_Fiscal_quarter 
    | FN_Fiscal_year | FN_Hour_in_day | FN_Week_in_month | FN_Week_in_year

    -- Aggregate Functions
    | FN_Avg | FN_Count | FN_Count_distinct | FN_Min | FN_Max | FN_Sum

    -- Location Functions
    | FN_Distance | FN_Geolocation

    --Other Functions
    | FN_Format | FN_Tolabel | FN_Convert_time_zone | FN_Convert_currency | FN_Grouping

    -- Symbols
    | Dot | Colon | Semicolon | Comma | Asterisk | Rparen | Lparen | Plus | Minus

    -- Date Literals
    | Yesterday | Today | Tomorrow | Last_week | This_week | Next_week | Last_month | This_month 
    | Next_month | Last_90_days | Next_90_days | This_quarter | Last_quarter | Next_quarter | This_year 
    | Last_year | Next_year | This_fiscal_quarter | Last_fiscal_quarter | Next_fiscal_quarter | This_fiscal_year 
    | Last_fiscal_year | Next_fiscal_year

    -- Date Literals
    | Next_n_days | Last_n_days | N_days_ago | Next_n_weeks | Last_n_weeks | N_weeks_ago | Next_n_months 
    | Last_n_months | N_months_ago | Next_n_quarters | Last_n_quarters | N_quarters_ago | Next_n_years | Last_n_years 
    | N_years_ago | Next_n_fiscal_quarters | Last_n_fiscal_quarters | N_fiscal_quarters_ago | Next_n_fiscal_years | Last_n_fiscal_years | N_fiscal_years_ago

    -- Identifiers
    | Name String 


derive instance genericToken :: Generic Token _ 

instance showToken :: Show Token where
    show = genericShow