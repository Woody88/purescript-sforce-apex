module Language.SOQL.Syntax where 

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Language.SOQL.Syntax.Types 

type Alias = Name 

-- A field can be a single Name or a Ref of names (i.e: Account.Lead)
type Field = List Name 

-- A Function Name with a list of its parameters (i.e: ROLLUP(Status, LeadSource) | FORMAT(convertCurrency(amount)) )
type FunctionExpr = Tuple FunctionName (List FunctionParameter)

data FunctionName = DateF FnDate | AggrF FnAggregate | LocF FnLocation | MiscF FnMisc

data FunctionParameter = FieldP Field | FuncP FunctionExpr

type SelectExpr = List (Tuple SelectClause (Maybe Alias))

data SelectClause = FOF FieldOrFunc | TypeOf TypeofExpr 

type ObjectTypeExpr = List (Tuple Field (Maybe Alias)) 

data FieldExpr = FieldExpr FieldOrFunc CompirasonOperator Value 

data FieldOrFunc = Field Field | Func FunctionExpr

data SetExpr = SetExpr FieldOrFunc CompirasonOperator (List Value) 

data LogicalExpr = LogicalExpr FieldExpr LogicalOperator (Maybe FieldExpr)

data ConditionExpr = LogicExpr LogicalExpr | SimplExpr SimpleExpr 

data SimpleExpr = CondExpr ConditionExpr | FldExpr FieldExpr | SExpr SetExpr 

data UsingExpr = Delegated | Everything | Mine | MineAndMyGroups | MyTerritory | MyTeamTerritory | Team

data OrderByExpr = OrderByExpr FieldOrderByList (Maybe OrderByProps) (Maybe OrderByNull)

data OrderByClause = FieldO Field | FuncO FunctionExpr 

type FieldOrderByList = List (Tuple OrderByClause (Maybe Alias))

-- Specifies whether the results are ordered in ascending (ASC) or descending (DESC) order. Default order is ascending.
data OrderByProps = Asc | Desc 

-- Orders null records at the beginning (NULLS FIRST) or end (NULLS LAST) of the results. By default, null values are sorted first. 
data OrderByNull = First | Last 

type LimitExpr = Value 

type OffsetExpr = Value 

data UpdateExpr = Tracking | ViewStat

data ForExpr = View | Reference | Update 

data FilterSelector = At | Above | Below | Above_Or_Below

data DataCategorySelection = DataCategorySelection Field FilterSelector Field 

type WithExpr = FilterExpr

data FilterExpr = FilterExpr DataCategorySelection (Maybe (Tuple LogicalOperator FilterExpr))

data TypeofExpr = TypeofExpr Name (List WhenThenClause) ElseClause

type WhenThenClause = Tuple Name (List Field)

type ElseClause = List Field 

data GroupByExpr = FieldG (List Field) | Rollup (List Field) | Cube (List Field)

type Query 
    = { select  :: SelectExpr 
      , from    :: ObjectTypeExpr
      , where   :: Maybe ConditionExpr
      , with    :: Maybe WithExpr
      , using   :: Maybe UsingExpr 
      , orderBy :: Maybe OrderByExpr
      , groupBy :: Maybe GroupByExpr
      , having  :: Maybe ConditionExpr
      , limit   :: Maybe LimitExpr
      , offset  :: Maybe OffsetExpr
      , for     :: Maybe (List ForExpr)
      , update  :: Maybe (List UpdateExpr)
      } 

derive instance genericFieldOrFunc :: Generic FieldOrFunc _ 
derive instance genericFieldExpr :: Generic FieldExpr _ 
derive instance genericSetExpr :: Generic SetExpr _ 
derive instance genericLogicalExpr :: Generic LogicalExpr _ 
derive instance genericConditionExpr :: Generic ConditionExpr _ 
derive instance genericSimpleExpr :: Generic SimpleExpr _ 
derive instance genericUsingExpr :: Generic UsingExpr _ 
derive instance genericOrderByExpr :: Generic OrderByExpr _ 
derive instance genericOrderByClause :: Generic OrderByClause _ 
derive instance genericOrderByProps :: Generic OrderByProps _ 
derive instance genericOrderByNull :: Generic OrderByNull _ 
derive instance genericUpdateExpr :: Generic UpdateExpr _ 
derive instance genericForExpr :: Generic ForExpr _ 
derive instance genericFilterSelector :: Generic FilterSelector _ 
derive instance genericDataCategorySelection :: Generic DataCategorySelection _
derive instance genericFilterExpr :: Generic FilterExpr _ 
derive instance genericFunctionName :: Generic FunctionName _ 
derive instance genericFunctionParameter :: Generic FunctionParameter _ 
derive instance genericSelectClause :: Generic SelectClause _ 
derive instance genericTypeofExpr :: Generic TypeofExpr _ 
derive instance genericGroupByExpr :: Generic GroupByExpr _ 

derive instance eqFieldOrFunc :: Eq FieldOrFunc
derive instance eqFieldExpr :: Eq FieldExpr 
derive instance eqSetExpr :: Eq SetExpr 
derive instance eqLogicalExpr :: Eq LogicalExpr 
derive instance eqConditionExpr :: Eq ConditionExpr 
derive instance eqSimpleExpr :: Eq SimpleExpr 
derive instance eqUsingExpr :: Eq UsingExpr 
derive instance eqOrderByExpr :: Eq OrderByExpr 
derive instance eqOrderByClause :: Eq OrderByClause 
derive instance eqOrderByProps :: Eq OrderByProps 
derive instance eqOrderByNull :: Eq OrderByNull 
derive instance eqUpdateExpr :: Eq UpdateExpr 
derive instance eqForExpr :: Eq ForExpr 
derive instance eqFilterSelector :: Eq FilterSelector 
derive instance eqDataCategorySelection :: Eq DataCategorySelection 
derive instance eqFilterExpr :: Eq FilterExpr 
derive instance eqFunctionName :: Eq FunctionName 
derive instance eqFunctionParameter :: Eq FunctionParameter 
derive instance eqSelectClause :: Eq SelectClause 
derive instance eqTypeofExpr :: Eq TypeofExpr 
derive instance eqGroupByExpr :: Eq GroupByExpr 

instance showFieldOrFunc :: Show FieldOrFunc where 
    show = genericShow 

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

instance showUpdateExpr :: Show UpdateExpr where 
    show = genericShow 

instance showForExpr :: Show ForExpr where 
    show = genericShow 

instance showFilterSelector :: Show FilterSelector where 
    show = genericShow

instance showDataCategorySelection :: Show DataCategorySelection where 
    show = genericShow 

instance showFilterExpr :: Show FilterExpr where 
    show x = genericShow x

instance showFunctionName :: Show FunctionName where 
    show x = genericShow x

instance showFunctionParameter :: Show FunctionParameter where 
    show x = genericShow x
    
instance showSelectClause :: Show SelectClause where 
    show x = genericShow x
    
instance showTypeofExpr :: Show TypeofExpr where 
    show x = genericShow x

instance showOrderByClause :: Show OrderByClause where 
    show x = genericShow x

instance showGroupByExpr :: Show GroupByExpr where 
    show x = genericShow x