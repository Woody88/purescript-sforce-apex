module Language.SOQL.Parser.Internal where 

import Prelude (($), (<$>), (>>=), (<>), show, pure, bind)
import Control.Alt ((<|>))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Language.Internal (langToken)
import Language.Types 
import Language.SOQL.Syntax.Types 
import Text.Parsing.Parser (Parser, fail)

type P = Parser (List (L Token)) 

value :: P Value 
value = dateform <|> value'
    where 
        dateform = DateFormula <$> dateformula
        value' = langToken $ \t -> case t of
            LiteralTok (IntegerTok i)  -> Just $ Integer i 
            LiteralTok (LongTok l)     -> Just $ Long l 
            LiteralTok (DoubleTok n)   -> Just $ Double n
            LiteralTok (DateTok d)     -> Just $ Date d 
            LiteralTok (DatetimeTok d) -> Just $ Datetime d
            LiteralTok (StringTok s)   -> Just $ String s
            LiteralTok (BoolTok b)     -> Just $ Boolean b 
            LiteralTok NullTok         -> Just $ Null 
            _             -> Nothing 

loperator :: P LogicalOperator 
loperator = langToken $ \t -> case t of
    OperatorTok Op_And -> Just $ AND 
    OperatorTok Op_Or  -> Just $ OR 
    SOQLExtraOpTok Op_Not -> Just $ NOT
    _      -> Nothing 

coperator :: P CompirasonOperator
coperator = langToken $ \t -> case t of
    OperatorTok Op_Equal       -> Just $ EQ
    SOQLExtraOpTok Op_NotEq    -> Just $ NEQ 
    OperatorTok Op_GThan    -> Just $ GT 
    OperatorTok Op_LThan    -> Just $ LT 
    OperatorTok Op_LThanE   -> Just $ LTE 
    OperatorTok Op_GThanE   -> Just $ GTE 
    SOQLExtraOpTok Op_In       -> Just $ IN 
    SOQLExtraOpTok Op_NotIn    -> Just $ NIN 
    SOQLExtraOpTok Op_Like     -> Just $ LIKE 
    SOQLExtraOpTok Op_Excludes -> Just $ EXCLUDES
    SOQLExtraOpTok Op_Includes -> Just $ INCLUDES
    _           -> Nothing 

fnDate :: P FnDate
fnDate = langToken $ \t -> case t of 
    SOQLDatFuncTok KW_Calendar_month   -> Just $ CALENDAR_MONTH 
    SOQLDatFuncTok KW_Calendar_quarter -> Just $ CALENDAR_QUARTER
    SOQLDatFuncTok KW_Calendar_year    -> Just $ CALENDAR_YEAR
    SOQLDatFuncTok KW_Day_in_month     -> Just $ DAY_IN_MONTH
    SOQLDatFuncTok KW_Day_in_week      -> Just $ DAY_IN_WEEK
    SOQLDatFuncTok KW_Day_in_year      -> Just $ DAY_IN_YEAR
    SOQLDatFuncTok KW_Day_only         -> Just $ DAY_ONLY
    SOQLDatFuncTok KW_Fiscal_month     -> Just $ FISCAL_MONTH
    SOQLDatFuncTok KW_Fiscal_quarter   -> Just $ FISCAL_QUARTER
    SOQLDatFuncTok KW_Fiscal_year      -> Just $ FISCAL_YEAR
    SOQLDatFuncTok KW_Hour_in_day      -> Just $ HOUR_IN_DAY
    SOQLDatFuncTok KW_Week_in_month    -> Just $ WEEK_IN_MONTH
    SOQLDatFuncTok KW_Week_in_year     -> Just $ WEEK_IN_YEAR
    _                   -> Nothing

fnAggregate :: P FnAggregate
fnAggregate = langToken $ \t -> case t of  
    SOQLAggrFuncTok KW_Count          -> Just $ COUNT
    SOQLAggrFuncTok KW_Avg            -> Just $ AVG 
    SOQLAggrFuncTok KW_Count_distinct -> Just $ COUNT_DISTINCT
    SOQLAggrFuncTok KW_Min            -> Just $ MIN
    SOQLAggrFuncTok KW_Max            -> Just $ MAX
    SOQLAggrFuncTok KW_Sum            -> Just $ SUM
    _                 -> Nothing

fnLocation :: P FnLocation
fnLocation = langToken $ \t -> case t of  
    SOQLLocFuncTok KW_Distance    -> Just $ DISTANCE
    SOQLLocFuncTok KW_Geolocation -> Just $ GEOLOCATION 
    _              -> Nothing

fnMisc :: P FnMisc
fnMisc = langToken $ \t -> case t of 
    SOQLMiscFuncTok KW_Format            -> Just $ FORMAT
    SOQLMiscFuncTok KW_Tolabel           -> Just $ TOLABEL 
    SOQLMiscFuncTok KW_Convert_time_zone -> Just $ CONVERT_TIME_ZONE
    SOQLMiscFuncTok KW_Convert_currency  -> Just $ CONVERT_CURRENCY
    SOQLMiscFuncTok KW_Grouping          -> Just $ GROUPING
    _                    -> Nothing

getIdent = langToken $ \t -> case t of 
    IdentTok (IdentT x) -> Just x
    _       -> Nothing

dateformula :: P DateFormula 
dateformula = dateFunction <|> dateLiteral

dateFunction :: P DateFormula
dateFunction = getIdent >>= \t ->  case toLower t of 
    "yesterday"               -> pure $ YESTERDAY 
    "today"                   -> pure $ TODAY 
    "tomorrow"                -> pure $ TOMORROW 
    "last_week"               -> pure $ LAST_WEEK 
    "this_week"               -> pure $ THIS_WEEK 
    "next_week"               -> pure $ NEXT_WEEK 
    "last_month"              -> pure $ LAST_MONTH 
    "this_month"              -> pure $ THIS_MONTH 
    "next_month"              -> pure $ NEXT_MONTH 
    "last_90_days"            -> pure $ LAST_90_DAYS 
    "next_90_days"            -> pure $ NEXT_90_DAYS 
    "this_quarter"            -> pure $ THIS_QUARTER 
    "last_quarter"            -> pure $ LAST_QUARTER 
    "next_quarter"            -> pure $ NEXT_QUARTER 
    "this_year"               -> pure $ THIS_YEAR 
    "last_year"               -> pure $ LAST_YEAR 
    "next_year"               -> pure $ NEXT_YEAR 
    "this_fiscal_quarter"      -> pure $ THIS_FISCAL_QUARTER 
    "last_fiscal_quarter"      -> pure $ LAST_FISCAL_QUARTER 
    "next_fiscal_quarter"      -> pure $ NEXT_FISCAL_QUARTER 
    "this_fiscal_year"         -> pure $ THIS_FISCAL_YEAR 
    "last_fiscal_year"         -> pure $ LAST_FISCAL_YEAR 
    "next_fiscal_year"         -> pure $ NEXT_FISCAL_YEAR 
    _                       -> fail ("unexpected dateformula: " <>  show t)
 
dateLiteral :: P DateFormula
dateLiteral = langToken $ \t -> case t of
    DateLiteralTok (Next_n_days i)           -> Just $ NEXT_N_DAYS i
    DateLiteralTok (Last_n_days i)           -> Just $ LAST_N_DAYS i
    DateLiteralTok (N_days_ago i)            -> Just $ N_DAYS_AGO i
    DateLiteralTok (Next_n_weeks i)          -> Just $ NEXT_N_WEEKS i
    DateLiteralTok (Last_n_weeks i)          -> Just $ LAST_N_WEEKS i
    DateLiteralTok (N_weeks_ago i)           -> Just $ N_WEEKS_AGO i
    DateLiteralTok (Next_n_months i)         -> Just $ NEXT_N_MONTHS i
    DateLiteralTok (Last_n_months i)         -> Just $ LAST_N_MONTHS i
    DateLiteralTok (N_months_ago i)          -> Just $ N_MONTHS_AGO i
    DateLiteralTok (Next_n_quarters i)       -> Just $ NEXT_N_QUARTERS i
    DateLiteralTok (Last_n_quarters i)       -> Just $ LAST_N_QUARTERS i
    DateLiteralTok (N_quarters_ago i)        -> Just $ N_QUARTERS_AGO i
    DateLiteralTok (Next_n_years i)          -> Just $ NEXT_N_YEARS i
    DateLiteralTok (Last_n_years i)          -> Just $ LAST_N_YEARS i
    DateLiteralTok (N_years_ago i)           -> Just $ N_YEARS_AGO i
    DateLiteralTok (Next_n_fiscal_quarters i) -> Just $ NEXT_N_FISCAL_QUARTERS i 
    DateLiteralTok (Last_n_fiscal_quarters i) -> Just $ LAST_N_FISCAL_QUARTERS i 
    DateLiteralTok (N_fiscal_quarters_ago i)  -> Just $ N_FISCAL_QUARTERS_AGO i 
    DateLiteralTok (Next_n_fiscal_years i)    -> Just $ NEXT_N_FISCAL_YEARS i 
    DateLiteralTok (Last_n_fiscal_years i)    -> Just $ LAST_N_FISCAL_YEARS i 
    DateLiteralTok (N_fiscal_years_ago i)     -> Just $ N_FISCAL_YEARS_AGO i
    _                       -> Nothing

