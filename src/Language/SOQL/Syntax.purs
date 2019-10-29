module Language.SOQL.Syntax where 

import Prelude
import Data.List (List)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)


newtype Name = Name String 

data Literal 
    = String String 
    | Number Number 
    | Date String 
    | Datetime String 
    | Boolean Boolean 
    | DateFormula DateFormula  
    | Null 

data LogicalOperator 
    = AND 
    | OR 
    | NOT 

data CompirasonOperator 
    = EQ
    | NEQ
    | LESS
    | GREATER 
    | LT
    | GT 
    | LIKE
    | IN
    | NIN 
    | INCLUDES
    | EXCLUDES 

data DateFormula 
    = YESTERDAY | TODAY | TOMORROW | LAST_WEEK | THIS_WEEK | NEXT_WEEK | LAST_MONTH | THIS_MONTH 
    | NEXT_MONTH | LAST_90_DAYS | NEXT_90_DAYS | THIS_QUARTER | LAST_QUARTER | NEXT_QUARTER | THIS_YEAR 
    | LAST_YEAR | NEXT_YEAR | THIS_FISCAL_QUARTER | LAST_FISCAL_QUARTER | NEXT_FISCAL_QUARTER | THIS_FISCAL_YEAR 
    | LAST_FISCAL_YEAR | NEXT_FISCAL_YEAR

    -- DATE N LITERALS
    | NEXT_N_DAYS Int | LAST_N_DAYS Int | N_DAYS_AGO Int | NEXT_N_WEEKS Int | LAST_N_WEEKS Int | N_WEEKS_AGO Int| NEXT_N_MONTHS Int
    | LAST_N_MONTHS Int | N_MONTHS_AGO Int | NEXT_N_QUARTERS Int | LAST_N_QUARTERS Int | N_QUARTERS_AGO Int | NEXT_N_YEARS Int 
    | LAST_N_YEARS Int | N_YEARS_AGO Int | NEXT_N_FISCAL_QUARTERS Int | LAST_N_FISCAL_QUARTERS Int | N_FISCAL_QUARTERS_AGO Int 
    | NEXT_N_FISCAL_YEARS Int | LAST_N_FISCAL_YEARS Int | N_FISCAL_YEARS_AGO Int

type FieldOrderByList = List Name 

data FieldExpr = FieldExpr Name CompirasonOperator Literal 


derive instance genericDateformula :: Generic DateFormula _ 
derive instance genericCompirasonOperator :: Generic CompirasonOperator _ 
derive instance genericLiteral :: Generic Literal _ 
derive instance genericName :: Generic Name _ 

instance showDateformula :: Show DateFormula where 
    show = genericShow 

instance showCompirasonOperator :: Show CompirasonOperator where 
    show = genericShow 
    
instance showLiteral :: Show Literal where 
    show = genericShow 

instance showName :: Show Name where 
    show = genericShow 