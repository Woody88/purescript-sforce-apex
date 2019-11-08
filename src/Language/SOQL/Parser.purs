module Language.SOQL.Parser where

import Prelude (Unit, ($), (<$>), (<*>), (*>), (>>=), (<<<), (==), (<>), unit, discard, bind, pure)
import Control.Applicative ((<*))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Foldable (for_)
import Data.Either (Either)
import Data.List.Lazy (List, singleton)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import Language.Internal (langToken, tok, seplist, seplist1, lopt, list, list1, optMaybe)
import Language.Types (L, Token(..))
import Language.Lexer (lexer)
import Language.SOQL.Syntax 
import Language.SOQL.Syntax.Types
import Language.SOQL.Parser.Internal 
import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.String (eof)
import Text.Parsing.Parser.Combinators ((<?>), optional, try, notFollowedBy, between)

parse :: forall a. String -> P a ->  Either ParseError a 
parse s p = runParser (lexer s) p

queryCompilation :: P Query 
queryCompilation = do 
    select  <- selectExpr
    from    <- fromExpr
    using   <- optMaybe usingExpr
    where_  <- optMaybe whereExpr
    with    <- optMaybe withExpr
    groupBy <- optMaybe groupByExpr
    having  <- optMaybe havingExpr
    orderBy <- optMaybe orderByExpr
    limit   <- optMaybe limitExpr 
    offset  <- optMaybe offsetExpr 
    update  <- optMaybe updateExpr
    for     <- optMaybe forExpr 
    pure $ {select, from, "where": where_, with, using, orderBy, groupBy, having, limit, offset, update, for}

selectExpr :: P SelectExpr
selectExpr = do  
    tok KW_Select
    let selectParam = Tuple <$> selectClause <*> optMaybe name 
    seplist1 selectParam comma
    
selectClause :: P SelectClause 
selectClause = try typeOf <|> fldOrFunc
    where 
        fldOrFunc = FOF <$> fieldOrFunc
        typeOf = TypeOf <$> typeofExpr

fromExpr :: P ObjectTypeExpr
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
        scope n@(Name s) = if toLower s == "scope" then pure unit else fail "scope token"

orderByExpr :: P OrderByExpr
orderByExpr = do 
    tok KW_OrderBy 
    let orderByParam = Tuple <$> orderByClause <*> optMaybe name 
    fldOrderList  <- seplist1 orderByParam comma
    orderByProps <- try (optMaybe ((tok KW_Asc *> pure Asc) <|> (tok KW_Desc *> pure Desc)))
    orderByNulls <- try (optMaybe ((tok KW_NullFirst *> pure First) <|> (tok KW_NullLast *> pure Last)))
    pure $ OrderByExpr fldOrderList orderByProps orderByNulls

orderByClause :: P OrderByClause
orderByClause = do 
    try simplField <|> funcField
    where 
        simplField = FieldO <$> field 
        funcField = FuncO <$> functionExpr

groupByExpr :: P GroupByExpr 
groupByExpr = try simplField <|> try rollup <|> cube
    where 
        simplField = tok KW_GroupBy *> (FieldG <$> (seplist1 field comma)) 
        rollup = tok KW_GroupByRollup *> (Rollup <$> (parens $ seplist1 field comma))
        cube = tok KW_GroupByCube *> (Cube <$> (parens $ seplist1 field comma)) 

havingExpr :: P ConditionExpr
havingExpr = do 
    tok KW_Having
    condExpr

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

withExpr :: P WithExpr
withExpr = do 
    tok' "with"
    optional (tok' "data" *> tok' "category")
    filterExpr

typeofExpr :: P TypeofExpr
typeofExpr = do 
    tok' "typeof" 
    typeOfField   <- name
    whenThen      <- list1 whenThenClause
    tok KW_Else 
    elseFieldList <- seplist field comma
    tok KW_End 
    pure $ TypeofExpr typeOfField whenThen elseFieldList

whenThenClause :: P WhenThenClause 
whenThenClause = do
    tok KW_When 
    whenObjectType <- name
    tok KW_Then 
    whenFieldList <- seplist1 field comma
    pure $ Tuple whenObjectType whenFieldList

filterExpr :: P FilterExpr
filterExpr = do 
    a <- dataCategorySelection
    try (loopList a) <|> (pure $ FilterExpr a Nothing)
    where 
        loopList a' = do 
            l <- loperator
            as <- filterExpr
            pure  $ FilterExpr a' (Just $ Tuple l as)

dataCategorySelection :: P DataCategorySelection
dataCategorySelection  = 
    DataCategorySelection <$> field <*> filterSelector <*> field 

filterSelector :: P FilterSelector
filterSelector = try at <|> try above <|> try below <|> aboveOrBelow
    where 
        at = tok' "at" *> pure At 
        above = tok' "above" *> pure Above 
        below = tok' "below" *> pure Below 
        aboveOrBelow = tok' "above_or_below" *> pure Above_Or_Below

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
setExpr = SetExpr <$> fieldOrFunc <*> coperator <*> parens (seplist1 value comma)

fieldExpr :: P FieldExpr 
fieldExpr = FieldExpr <$> fieldOrFunc <*> coperator <*> value 

functionExpr :: P FunctionExpr
functionExpr = Tuple <$> functionName <*> parens (seplist (fix $ \_ -> functionParameter) comma)

functionName :: P FunctionName
functionName = 
    try (DateF <$> fnDate)      <|> 
    try (AggrF <$> fnAggregate) <|> 
    try (LocF <$> fnLocation)   <|> 
    (MiscF <$> fnMisc)

functionParameter :: P FunctionParameter 
functionParameter = try (FieldP <$> field) <|> (FuncP <$> (fix $ \_ -> functionExpr))

fieldOrFunc :: P FieldOrFunc
fieldOrFunc = try simplField <|> funcField
    where 
        simplField = Field <$> field
        funcField  = Func <$> functionExpr

valueList :: P (List Value)
valueList = seplist1 value comma

fieldList:: P (List (Tuple Field (Maybe Name)))
fieldList = seplist1 (Tuple <$> field <*> optMaybe name) comma 

field :: P Field 
field = seplist1 name period 

comma :: P Unit 
comma = tok Comma  

period :: P Unit 
period = tok Period 

parens :: forall a. P a -> P a 
parens = between (tok OpenParen) (tok CloseParen)

name :: P Name 
name = ident -- <* notFollowedBy period) -- <|>  refName <?> "name"

refName :: P (List Name) 
refName = seplist1 ident period 

ident :: P Name 
ident = langToken $ \t -> case t of
    IdentTok s -> Just $ Name s
    _ -> Nothing

tok' :: String -> P Unit
tok' t = token
    where 
        token = ident >>= tokenName
        tokenName n@(Name s) = if toLower s == t then pure unit else fail ("unexpected " <> t <> " token")

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