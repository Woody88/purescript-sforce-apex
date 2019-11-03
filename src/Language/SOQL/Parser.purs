module Language.SOQL.Parser where

import Prelude (Unit, ($), (<$>), (<*>), (*>), (>>=), (<<<), (==), (<>), unit, discard, bind, pure)
import Control.Applicative ((<*))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Foldable (for_)
import Data.Either (Either)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Language.Internal (langToken, tok, seplist1, lopt, list, optMaybe)
import Language.Types (L)
import Language.SOQL.Lexer (lexSOQL)
import Language.SOQL.Lexer.Types (Token(..))
import Language.SOQL.Syntax 
import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.String (eof)
import Text.Parsing.Parser.Combinators ((<?>), try, notFollowedBy, between)

type P = Parser (List (L Token))

parse :: forall a. String -> P a ->  Either ParseError a 
parse s p = runParser (lexSOQL s) p

queryCompilation :: P Query 
queryCompilation = do 
    select  <- selectExpr
    from    <- fromExpr
    using   <- optMaybe usingExpr
    where_  <- optMaybe whereExpr
    orderBy <- optMaybe orderByExpr
    limit   <- optMaybe limitExpr 
    offset  <- optMaybe offsetExpr 
    update  <- optMaybe updateExpr
    for     <- optMaybe forExpr 
    pure $ {select, from, "where": where_, using, orderBy, limit, offset, update, for}

selectExpr :: P (List Name)
selectExpr = do  
    tok KW_Select
    fieldList

fromExpr :: P (List Name)
fromExpr = do 
    tok KW_From
    fieldList

whereExpr :: P ConditionExpr
whereExpr = do 
    tok KW_Where
    condExpr
    
usingExpr :: P UsingExpr 
usingExpr = do 
    tok KW_Using 
    scopeTok
    filterScope
    where 
        scopeTok = ident >>= scope
        scope n@(Name s) = if toLower s == "scope" then pure unit else scope (Ref $ singleton n)
        scope (Ref n)    = fail "scope token"

orderByExpr :: P OrderByExpr
orderByExpr = do 
    tok KW_OrderBy 
    fldOrderList  <- fieldList
    orderByProps <- try (tok KW_Asc *> pure Asc) <|> (tok KW_Desc *> pure Desc)
    orderByNulls <- try (tok KW_NullFirst *> pure First) <|> (tok KW_NullLast *> pure Last)
    pure $ OrderByExpr fldOrderList orderByProps orderByNulls

limitExpr :: P LimitExpr
limitExpr = do 
    tok KW_Limit 
    value <?> "limitExpr"

offsetExpr :: P OffsetExpr 
offsetExpr = do 
    tok' "offset" 
    value <?> "OffsetExpr"

updateExpr :: P (List UpdateExpr)
updateExpr = do 
    tok KW_Update 
    seplist1 (try tracking <|> viewstat) comma 
    where 
        tracking = tok' "tracking" *> pure Tracking 
        viewstat = tok' "viewstat" *> pure ViewStat

forExpr :: P (List ForExpr)
forExpr = do 
    tok KW_For 
    seplist1 (try view <|> try reference <|> update) comma
    where 
        view = tok' "view" *> pure View 
        reference = tok' "reference" *> pure Reference  
        update = tok KW_Update *> pure Update 

condExpr :: P ConditionExpr
condExpr = 
    try (LogicExpr <$> logicalExpr) <|> 
    (SimplExpr <$> smplExp) <?> "codndExpr"
    where 
        smplExp = fix $ \_ -> simpleExpr


simpleExpr :: P SimpleExpr 
simpleExpr = 
    try (SExpr <$> setExpr)    <|>
    try (FldExpr <$> fieldExpr) <|>
    try (parens condExp)       <|>
    try condExp                <?> "simplExpr" 
    where 
        condExp = fix $ \_ -> (CondExpr <$> condExpr)
    

logicalExpr :: P LogicalExpr 
logicalExpr = 
    try notCase <|>
    LogicalExpr <$> fieldExpr <*> loperator <*> optMaybe fieldExpr
    where 
        notCase = do 
            lop <- loperator
            fexp <- fieldExpr
            pure $ LogicalExpr fexp lop Nothing

setExpr :: P SetExpr 
setExpr = SetExpr <$> name <*> coperator <*> parens (seplist1 value comma)

fieldExpr :: P FieldExpr 
fieldExpr = FieldExpr <$> name <*> coperator <*> value 

valueList :: P (List Value)
valueList = seplist1 value comma

fieldList :: P (List Name)
fieldList = seplist1 field comma 

field :: P Name 
field = name 

comma :: P Unit 
comma = tok Comma  

period :: P Unit 
period = tok Period 

parens :: forall a. P a -> P a 
parens = between (tok OpenParen) (tok CloseParen)

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
    Op_Eq       -> Just $ EQ
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

name :: P Name 
name = try (ident <* notFollowedBy period) <|>  refName <?> "name"

refName :: P Name 
refName = Ref <$> seplist1 ident period 

ident :: P Name 
ident = langToken $ \t -> case t of
    Ident s -> Just $ Name s
    _ -> Nothing

tok' :: String -> P Unit
tok' t = token
    where 
        token = ident >>= tokenName
        tokenName n@(Name s) = if toLower s == t then pure unit else tokenName (Ref $ singleton n)
        tokenName (Ref n)    = fail ("unexpected " <> t <> " token")

filterScope :: P UsingExpr 
filterScope = do 
    scope <- getIdent 
    case (toLower scope) of
        "delegated"         -> pure $ Delegated
        "everything"        -> pure $ Everything
        "mine"              -> pure $ Mine 
        "mineandmygroups"   -> pure $ MineAndMyGroups 
        "my_territory"      -> pure $ MyTerritory 
        "my_team_territory" -> pure $ MyTeamTerritory
        "team"              -> pure $ Team 
        _                   -> fail "unexepected filterScope token"

    where 
        getIdent = langToken $ \t -> case t of 
            Ident x -> Just x
            _       -> Nothing


dateformula :: P DateFormula 
dateformula = langToken $ \t -> case t of 
    Yesterday               -> Just $ YESTERDAY 
    Today                   -> Just $ TODAY 
    Tomorrow                -> Just $ TOMORROW 
    Last_week               -> Just $ LAST_WEEK 
    This_week               -> Just $ THIS_WEEK 
    Next_week               -> Just $ NEXT_WEEK 
    Last_month              -> Just $ LAST_MONTH 
    This_month              -> Just $ THIS_MONTH 
    Next_month              -> Just $ NEXT_MONTH 
    Last_90_days            -> Just $ LAST_90_DAYS 
    Next_90_days            -> Just $ NEXT_90_DAYS 
    This_quarter            -> Just $ THIS_QUARTER 
    Last_quarter            -> Just $ LAST_QUARTER 
    Next_quarter            -> Just $ NEXT_QUARTER 
    This_year               -> Just $ THIS_YEAR 
    Last_year               -> Just $ LAST_YEAR 
    Next_year               -> Just $ NEXT_YEAR 
    This_fiscal_quarter      -> Just $ THIS_FISCAL_QUARTER 
    Last_fiscal_quarter      -> Just $ LAST_FISCAL_QUARTER 
    Next_fiscal_quarter      -> Just $ NEXT_FISCAL_QUARTER 
    This_fiscal_year         -> Just $ THIS_FISCAL_YEAR 
    Last_fiscal_year         -> Just $ LAST_FISCAL_YEAR 
    Next_fiscal_year         -> Just $ NEXT_FISCAL_YEAR 
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
