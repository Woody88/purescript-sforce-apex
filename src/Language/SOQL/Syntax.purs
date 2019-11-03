module Language.SOQL.Syntax where 

import Prelude
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)


data Name 
    = Name String 
    | Ref (List Name) 

data Value 
    = String String 
    | Double Number 
    | Integer Int
    | Long BigInt
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
    | LTE 
    | GTE 
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


-- Specifies whether the results are ordered in ascending (ASC) or descending (DESC) order. Default order is ascending.
data OrderByProps 
    = ASC | DESC 

-- Orders null records at the beginning (NULLS FIRST) or end (NULLS LAST) of the results. By default, null values are sorted first. 
data OrderByNull = First | Last 

type FieldOrderByList = List Name 
type ObjectTypeList = List Name 

data FieldExpr = FieldExpr Name CompirasonOperator Value 

data SetExpr = SetExpr Name CompirasonOperator (List Value) 

data LogicalExpr = LogicalExpr FieldExpr LogicalOperator (Maybe FieldExpr)

data ConditionExpr = LogicExpr LogicalExpr | SimplExpr SimpleExpr 

data SimpleExpr = CondExpr ConditionExpr | FldExpr FieldExpr | SExpr SetExpr 

data UsingExpr = Delegated | Everything | Mine | MineAndMyGroups | MyTerritory | MyTeamTerritory | Team

data OrderByExpr = OrderByExpr FieldOrderByList OrderByProps OrderByNull

type LimitExpr = Value 

type OffsetExpr = Value 

type Query 
    = { select  :: FieldOrderByList 
      , from    :: ObjectTypeList
      , where   :: Maybe ConditionExpr
      , using   :: Maybe UsingExpr 
      , orderBy :: Maybe OrderByExpr
      , limit   :: Maybe LimitExpr
      , offset  :: Maybe OffsetExpr
      } 

derive instance genericDateformula :: Generic DateFormula _ 
derive instance genericCompirasonOperator :: Generic CompirasonOperator _ 
derive instance genericLogicalOperator :: Generic LogicalOperator _ 
derive instance genericValue :: Generic Value _ 
derive instance genericName :: Generic Name _ 
derive instance genericFieldExpr :: Generic FieldExpr _ 
derive instance genericSetExpr :: Generic SetExpr _ 
derive instance genericLogicalExpr :: Generic LogicalExpr _ 
derive instance genericConditionExpr :: Generic ConditionExpr _ 
derive instance genericSimpleExpr :: Generic SimpleExpr _ 
derive instance genericUsingExpr :: Generic UsingExpr _ 
derive instance genericOrderByExpr :: Generic OrderByExpr _ 
derive instance genericOrderByProps :: Generic OrderByProps _ 
derive instance genericOrderByNull :: Generic OrderByNull _ 

derive instance eqDateformula :: Eq DateFormula 
derive instance eqCompirasonOperator :: Eq CompirasonOperator 
derive instance eqLogicalOperator :: Eq LogicalOperator 
derive instance eqValue :: Eq Value 
derive instance eqName :: Eq Name 
derive instance eqFieldExpr :: Eq FieldExpr 
derive instance eqSetExpr :: Eq SetExpr 
derive instance eqLogicalExpr :: Eq LogicalExpr 
derive instance eqConditionExpr :: Eq ConditionExpr 
derive instance eqSimpleExpr :: Eq SimpleExpr 
derive instance eqUsingExpr :: Eq UsingExpr 
derive instance eqOrderByExpr :: Eq OrderByExpr 
derive instance eqOrderByProps :: Eq OrderByProps 
derive instance eqOrderByNull :: Eq OrderByNull 

instance showDateformula :: Show DateFormula where 
    show = genericShow 

instance showCompirasonOperator :: Show CompirasonOperator where 
    show = genericShow 

instance showLogicalOperator :: Show LogicalOperator where 
    show = genericShow 
    
instance showValue :: Show Value where 
    show = genericShow 

instance showName :: Show Name where 
    show (Name s) = "(Name " <> s <> ")"
    show x = genericShow x

instance showFieldExpr :: Show FieldExpr where 
    show = genericShow 

instance showSetExpr :: Show SetExpr where 
    show = genericShow 

instance showLogicalExpr :: Show LogicalExpr where 
    show = genericShow 

instance showConditionExpr :: Show ConditionExpr where 
    show (SimplExpr se) = "(SimplExpr " <> show se <> ")"
    show x = genericShow x

instance showSimpleExpr :: Show SimpleExpr where 
    show = genericShow 

instance showUsingExpr :: Show UsingExpr where 
    show = genericShow 

instance showOrderByExpr :: Show OrderByExpr where 
    show = genericShow 

instance showOrderByProps :: Show OrderByProps where 
    show = genericShow 

instance showOrderByNull :: Show OrderByNull where 
    show = genericShow 