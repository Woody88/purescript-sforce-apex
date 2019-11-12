module Language.SOQL.Parser.Internal where 

import Prelude (($), (<$>), (>>=), (<>), show, pure, bind)
import Control.Alt ((<|>))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Language.Internal (langToken)
import Language.Types (L, Token(..))
import Language.SOQL.Syntax.Types 
import Text.Parsing.Parser (Parser, fail)

type P = Parser (List (L Token)) 

value :: P Value 
value = dateform <|> value'
    where 
        dateform = DateFormula <$> dateformula
        value' = langToken $ \t -> case t of
            IntegerTok i  -> Just $ Integer i 
            LongTok l     -> Just $ Long l 
            DoubleTok n   -> Just $ Double n
            DateTok d     -> Just $ Date d 
            DatetimeTok d -> Just $ Datetime d
            StringTok s   -> Just $ String s
            BoolTok b     -> Just $ Boolean b 
            NullTok       -> Just $ Null 
            _             -> Nothing 

loperator :: P LogicalOperator 
loperator = langToken $ \t -> case t of
    Op_And -> Just $ AND 
    Op_Or  -> Just $ OR 
    Op_Not -> Just $ NOT
    _      -> Nothing 

coperator :: P CompirasonOperator
coperator = langToken $ \t -> case t of
    Op_Equal       -> Just $ EQ
    Op_NotEq    -> Just $ NEQ 
    Op_GThan    -> Just $ GT 
    Op_LThan    -> Just $ LT 
    Op_LThanE   -> Just $ LTE 
    Op_GThanE   -> Just $ GTE 
    Op_In       -> Just $ IN 
    Op_NotIn    -> Just $ NIN 
    Op_Like     -> Just $ LIKE 
    Op_Excludes -> Just $ EXCLUDES
    Op_Includes -> Just $ INCLUDES
    _           -> Nothing 

fnDate :: P FnDate
fnDate = langToken $ \t -> case t of 
    KW_Calendar_month   -> Just $ CALENDAR_MONTH 
    KW_Calendar_quarter -> Just $ CALENDAR_QUARTER
    KW_Calendar_year    -> Just $ CALENDAR_YEAR
    KW_Day_in_month     -> Just $ DAY_IN_MONTH
    KW_Day_in_week      -> Just $ DAY_IN_WEEK
    KW_Day_in_year      -> Just $ DAY_IN_YEAR
    KW_Day_only         -> Just $ DAY_ONLY
    KW_Fiscal_month     -> Just $ FISCAL_MONTH
    KW_Fiscal_quarter   -> Just $ FISCAL_QUARTER
    KW_Fiscal_year      -> Just $ FISCAL_YEAR
    KW_Hour_in_day      -> Just $ HOUR_IN_DAY
    KW_Week_in_month    -> Just $ WEEK_IN_MONTH
    KW_Week_in_year     -> Just $ WEEK_IN_YEAR
    _                   -> Nothing

fnAggregate :: P FnAggregate
fnAggregate = langToken $ \t -> case t of  
    KW_Count          -> Just $ COUNT
    KW_Avg            -> Just $ AVG 
    KW_Count_distinct -> Just $ COUNT_DISTINCT
    KW_Min            -> Just $ MIN
    KW_Max            -> Just $ MAX
    KW_Sum            -> Just $ SUM
    _                 -> Nothing

fnLocation :: P FnLocation
fnLocation = langToken $ \t -> case t of  
    KW_Distance    -> Just $ DISTANCE
    KW_Geolocation -> Just $ GEOLOCATION 
    _              -> Nothing

fnMisc :: P FnMisc
fnMisc = langToken $ \t -> case t of 
    KW_Format            -> Just $ FORMAT
    KW_Tolabel           -> Just $ TOLABEL 
    KW_Convert_time_zone -> Just $ CONVERT_TIME_ZONE
    KW_Convert_currency  -> Just $ CONVERT_CURRENCY
    KW_Grouping          -> Just $ GROUPING
    _                    -> Nothing

getIdent = langToken $ \t -> case t of 
    IdentTok x -> Just x
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
    Next_n_days i           -> Just $ NEXT_N_DAYS i
    Last_n_days i           -> Just $ LAST_N_DAYS i
    N_days_ago i            -> Just $ N_DAYS_AGO i
    Next_n_weeks i          -> Just $ NEXT_N_WEEKS i
    Last_n_weeks i          -> Just $ LAST_N_WEEKS i
    N_weeks_ago i           -> Just $ N_WEEKS_AGO i
    Next_n_months i         -> Just $ NEXT_N_MONTHS i
    Last_n_months i         -> Just $ LAST_N_MONTHS i
    N_months_ago i          -> Just $ N_MONTHS_AGO i
    Next_n_quarters i       -> Just $ NEXT_N_QUARTERS i
    Last_n_quarters i       -> Just $ LAST_N_QUARTERS i
    N_quarters_ago i        -> Just $ N_QUARTERS_AGO i
    Next_n_years i          -> Just $ NEXT_N_YEARS i
    Last_n_years i          -> Just $ LAST_N_YEARS i
    N_years_ago i           -> Just $ N_YEARS_AGO i
    Next_n_fiscal_quarters i -> Just $ NEXT_N_FISCAL_QUARTERS i 
    Last_n_fiscal_quarters i -> Just $ LAST_N_FISCAL_QUARTERS i 
    N_fiscal_quarters_ago i  -> Just $ N_FISCAL_QUARTERS_AGO i 
    Next_n_fiscal_years i    -> Just $ NEXT_N_FISCAL_YEARS i 
    Last_n_fiscal_years i    -> Just $ LAST_N_FISCAL_YEARS i 
    N_fiscal_years_ago i     -> Just $ N_FISCAL_YEARS_AGO i
    _                       -> Nothing

