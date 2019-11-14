module Language.Core where 

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.Lazy (fix)
import Data.HashSet as HS
import Data.Int as Int
import Data.Either (Either(..))
import Data.BigInt as BigInt
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse, sequence)
import Data.Array as Array
import Data.Char.Unicode (isAlpha, toLower, toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.List (List, toUnfoldable, fromFoldable, many)
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits (fromCharArray, singleton)
import Language.Types 
import Language.Internal
import Text.Parsing.Parser (ParseError, runParser, position, fail, parseErrorMessage)
import Text.Parsing.Parser.Combinators 
import Text.Parsing.Parser.Language 
import Text.Parsing.Parser.String 
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser)
import Text.Parsing.Parser.Token 

readToken :: P (L Token)
readToken = 
    try (SOQLTok                <<=: testSOQL)                 <|>     
    try ((LiteralTok <<< DatetimeTok)            <<=: datetimeLiteral)          <|>
    try ((LiteralTok <<< DateTok)                <<=: dateLiteral)              <|>
    try ((LiteralTok <<< LongTok)                <<=: longLiteral)              <|>
    try ((LiteralTok <<< DoubleTok)              <<=: doubleLiteral)            <|>
    try ((LiteralTok <<< IntegerTok)             <<=: integerLiteral)           <|>
    try ((LiteralTok <<< StringTok)              <<=: stringLiteral)            <|>
    try ((LiteralTok <<< BoolTok)                <<=: boolLiteral)              <|>
    try (SymbolTok Period                 <=:  period     )              <|>

    -- SOQL Date Related Token 
    try ((DateLiteralTok <<< Next_n_days)             <<=: strNDate "next_n_days" )     <|> 
    try ((DateLiteralTok <<< Last_n_days)             <<=: strNDate "last_n_days" )     <|> 
    try ((DateLiteralTok <<< N_days_ago)              <<=: strNDate "n_days_ago" )      <|> 
    try ((DateLiteralTok <<< Next_n_weeks)            <<=: strNDate "next_n_weeks" )    <|> 
    try ((DateLiteralTok <<< Last_n_weeks)            <<=: strNDate "last_n_weeks" )    <|> 
    try ((DateLiteralTok <<< N_weeks_ago)             <<=: strNDate "n_weeks_ago" )     <|>
    try ((DateLiteralTok <<< Next_n_months)           <<=: strNDate "next_n_months" )   <|>
    try ((DateLiteralTok <<< Last_n_months)           <<=: strNDate "last_n_months" )   <|> 
    try ((DateLiteralTok <<< N_months_ago)            <<=: strNDate "n_months_ago" )    <|> 
    try ((DateLiteralTok <<< Next_n_quarters)         <<=: strNDate "next_n_quarters" ) <|> 
    try ((DateLiteralTok <<< Last_n_quarters)         <<=: strNDate "last_n_quarters" ) <|> 
    try ((DateLiteralTok <<< N_quarters_ago)          <<=: strNDate "n_quarters_ago" )  <|> 
    try ((DateLiteralTok <<< Next_n_years)            <<=: strNDate "next_n_years" )    <|> 
    try ((DateLiteralTok <<< Last_n_years)            <<=: strNDate "last_n_years" )    <|> 
    try ((DateLiteralTok <<< N_years_ago)             <<=: strNDate "n_years_ago" )     <|> 
    try ((DateLiteralTok <<< Next_n_fiscal_quarters)  <<=: strNDate "next_n_fiscal_quarters" ) <|> 
    try ((DateLiteralTok <<< Last_n_fiscal_quarters)  <<=: strNDate "last_n_fiscal_quarters" ) <|> 
    try ((DateLiteralTok <<< N_fiscal_quarters_ago)   <<=: strNDate "n_fiscal_quarters_ago" )  <|> 
    try ((DateLiteralTok <<< Next_n_fiscal_years)     <<=: strNDate "next_n_fiscal_years" )    <|> 
    try ((DateLiteralTok <<< Last_n_fiscal_years)     <<=: strNDate "last_n_fiscal_years" )    <|> 
    try ((DateLiteralTok <<< N_fiscal_years_ago)      <<=: strNDate "n_fiscal_years_ago" )     <|>
    -- try (Yesterday            <=: istring "yesterday" )         <|> 
    -- try (Today                <=: istring "today" )             <|> 
    -- try (Tomorrow             <=: istring "tomorrow" )          <|> 
    -- try (Last_week            <=: istring "last_week" )         <|> 
    -- try (This_week            <=: istring "this_week" )         <|> 
    -- try (Next_week            <=: istring "next_week" )         <|> 
    -- try (Last_month           <=: istring "last_month" )        <|> 
    -- try (This_month           <=: istring "this_month" )        <|> 
    -- try (Next_month           <=: istring "next_month" )        <|> 
    -- try (Last_90_days         <=: istring "last_90_days" )      <|> 
    -- try (Next_90_days         <=: istring "next_90_days" )      <|> 
    -- try (This_quarter         <=: istring "this_quarter" )      <|> 
    -- try (Last_quarter         <=: istring "last_quarter" )      <|> 
    -- try (Next_quarter         <=: istring "next_quarter" )      <|> 
    -- try (This_year            <=: istring "this_year" )         <|> 
    -- try (Last_year            <=: istring "last_year" )         <|> 
    -- try (Next_year            <=: istring "next_year" )         <|> 
    -- try (This_fiscal_quarter  <=: istring "this_fiscal_quarter" ) <|> 
    -- try (Last_fiscal_quarter  <=: istring "last_fiscal_quarter" ) <|> 
    -- try (Next_fiscal_quarter  <=: istring "next_fiscal_quarter" ) <|> 
    -- try (This_fiscal_year     <=: istring "this_fiscal_year"    ) <|> 
    -- try (Last_fiscal_year     <=: istring "last_fiscal_year"    ) <|> 
    -- try (Next_fiscal_year     <=: istring "next_fiscal_year"    ) <|>
    try (SOQLMiscFuncTok KW_Format               <=: funcLiteral "format"  )    <|> 
    try (SOQLMiscFuncTok KW_Tolabel              <=: funcLiteral "tolabel" )    <|> 
    try (SOQLMiscFuncTok KW_Convert_time_zone    <=: funcLiteral "converttimezone" ) <|> 
    try (SOQLMiscFuncTok KW_Convert_currency     <=: funcLiteral "convertcurrency" ) <|> 
    try (SOQLMiscFuncTok KW_Grouping             <=: funcLiteral "grouping" )        <|> 
    try (SOQLLocFuncTok KW_Distance             <=: funcLiteral "distance" )        <|> 
    try (SOQLLocFuncTok KW_Geolocation          <=: funcLiteral "geolocation" )     <|>
    try (SOQLAggrFuncTok KW_Avg                  <=: funcLiteral "avg" )             <|> 
    try (SOQLAggrFuncTok KW_Count                <=: funcLiteral "count" )           <|> 
    try (SOQLAggrFuncTok KW_Count_distinct       <=: funcLiteral "count_distinct" )  <|> 
    try (SOQLAggrFuncTok KW_Min                  <=: funcLiteral "min" )             <|> 
    try (SOQLAggrFuncTok KW_Max                  <=: funcLiteral "max" )             <|> 
    try (SOQLAggrFuncTok KW_Sum                  <=: funcLiteral "sum" )             <|>
    try (SOQLDatFuncTok KW_Calendar_month       <=: funcLiteral "calendar_month" )   <|> 
    try (SOQLDatFuncTok KW_Calendar_quarter     <=: funcLiteral "calendar_quarter" ) <|> 
    try (SOQLDatFuncTok KW_Calendar_year        <=: funcLiteral "calendar_year" )    <|> 
    try (SOQLDatFuncTok KW_Day_in_month         <=: funcLiteral "day_in_month" )     <|> 
    try (SOQLDatFuncTok KW_Day_in_week          <=: funcLiteral "day_in_week" )      <|> 
    try (SOQLDatFuncTok KW_Day_in_year          <=: funcLiteral "day_in_year" )      <|> 
    try (SOQLDatFuncTok KW_Day_only             <=: funcLiteral "day_only" )         <|> 
    try (SOQLDatFuncTok KW_Fiscal_month         <=: funcLiteral "fiscal_month" )      <|> 
    try (SOQLDatFuncTok KW_Fiscal_quarter       <=: funcLiteral "fiscal_quarter" )    <|> 
    try (SOQLDatFuncTok KW_Fiscal_year          <=: funcLiteral "fiscal_year" )       <|> 
    try (SOQLDatFuncTok KW_Hour_in_day          <=: funcLiteral "hour_in_day" )      <|> 
    try (SOQLDatFuncTok KW_Week_in_month        <=: funcLiteral "week_in_month" )    <|> 
    try (SOQLDatFuncTok KW_Week_in_year         <=: funcLiteral "week_in_year" )     <|>

    try (SharingTok KW_With_Share          <=: istring "with sharing" )   <|>
    try (SharingTok KW_Without_Share       <=: istring "without sharing") <|> 
    try (SharingTok KW_Inherit_Share       <=: istring "inherit sharing") <|>
    try (KeywordTok KW_WhenElse            <=: istring "when else")       <|> 
    try (KeywordTok KW_OrderBy             <=: istring "order by")        <|>  
    try (KeywordTok KW_Switch              <=: istring "switch on")       <|> 
    try (KeywordTok KW_NullFirst           <=: istring "nulls first")      <|>  
    try (KeywordTok KW_NullLast            <=: istring "nulls last")      <|>    
    try (KeywordTok KW_GroupByCube         <=: istring "group by cube"  ) <|>     
    try (KeywordTok KW_GroupByRollup       <=: istring "group by rollup") <|>  
    try (KeywordTok KW_GroupBy             <=: istring "group by")        <|>   
    try (SOQLExtraOpTok Op_NotIn               <=: istring "not in"  )        <|>

    -- Identifier     
    identTok                                                   <|>

    try (KeywordTok KW_Override            <=: istring "override" )       <|>
    try (KeywordTok KW_Object              <=: istring "object" )         <|>
    try (KeywordTok KW_Time                <=: istring "time")            <|> 
    try (KeywordTok KW_Date                <=: istring "date")            <|> 
    try (KeywordTok KW_Datetime            <=: istring "datetime")        <|>
    try (KeywordTok KW_When                <=: istring "when")            <|> 
    try (KeywordTok KW_Abstract            <=: istring "abstract")        <|> 
    try (KeywordTok KW_Integer             <=: istring "integer")         <|> 
    try (KeywordTok KW_String              <=: istring "string")          <|>
    try (KeywordTok KW_Boolean             <=: istring "boolean")         <|> 
    try (KeywordTok KW_Break               <=: istring "break")           <|> 
    try (KeywordTok KW_Blob                <=: istring "blob")            <|> 
    try (KeywordTok KW_Case                <=: istring "case")            <|> 
    try (KeywordTok KW_Catch               <=: istring "catch")           <|> 
    try (KeywordTok KW_Class               <=: istring "class")           <|> 
    try (KeywordTok KW_Const               <=: istring "const")           <|> 
    try (KeywordTok KW_Continue            <=: istring "continue")        <|> 
    try (KeywordTok KW_Double              <=: istring "double")          <|> 
    try (KeywordTok KW_Else                <=: istring "else")            <|> 
    try (KeywordTok KW_Enum                <=: istring "enum")            <|> 
    try (KeywordTok KW_Extends             <=: istring "extends")         <|> 
    try (KeywordTok KW_Final               <=: istring "final")            <|> 
    try (KeywordTok KW_Finally             <=: istring "finally")          <|> 
    try (KeywordTok KW_Decimal             <=: istring "decimal")         <|> 
    try (KeywordTok KW_Implements          <=: istring "implements")      <|> 
    try (KeywordTok KW_Import              <=: istring "import")          <|> 
    try (KeywordTok KW_Instanceof          <=: istring "instanceof")      <|> 
    try (KeywordTok KW_Interface           <=: istring "interface")       <|> 
    try (KeywordTok KW_Long                <=: istring "long")            <|> 
    try (KeywordTok KW_New                 <=: istring "new")             <|> 
    try (KeywordTok KW_Private             <=: istring "private")         <|> 
    try (KeywordTok KW_Virtual             <=: istring "virtual")         <|> 
    try (KeywordTok KW_Global              <=: istring "global")          <|> 
    try (KeywordTok KW_Protected           <=: istring "protected")       <|> 
    try (KeywordTok KW_Public              <=: istring "public")          <|> 
    try (KeywordTok KW_Return              <=: istring "return")          <|> 
    try (KeywordTok KW_Static              <=: istring "static")          <|> 
    try (KeywordTok KW_Super               <=: istring "super")           <|> 
    try (KeywordTok KW_This                <=: istring "this")            <|> 
    try (KeywordTok KW_Transient           <=: istring "transient")       <|> 
    try (KeywordTok KW_Try                 <=: istring "try")             <|> 
    try (KeywordTok KW_Void                <=: istring "void")            <|> 
    try (KeywordTok KW_While               <=: istring "while")           <|>  
    try (KeywordTok KW_Desc      <=: istring "desc"    )    <|>  
    try (KeywordTok KW_Then      <=: istring "then"    )    <|>  
    try (KeywordTok KW_End       <=: istring "end"    )     <|>  
    try (KeywordTok KW_Asc       <=: istring "asc"     )    <|>   
    try (KeywordTok KW_From      <=: istring "from"    )    <|>
    try (KeywordTok KW_Having    <=: istring "having"  )    <|>   
    try (KeywordTok KW_Limit     <=: istring "limit"   )    <|>    
    try (KeywordTok KW_Select    <=: istring "select"  )    <|>   
    try (KeywordTok KW_Using     <=: istring "using"   )    <|>   
    try (KeywordTok KW_Where     <=: istring "where"   )    <|>    

    -- DML Keywords
    try (DMLOpTok KW_Update    <=: istring "update"  )    <|> 
    try (DMLOpTok KW_Insert    <=: istring "insert"  )    <|> 
    try (DMLOpTok KW_Upsert    <=: istring "upsert"  )    <|> 
    try (DMLOpTok KW_Delete    <=: istring "delete"  )    <|> 
    try (DMLOpTok KW_Undelete  <=: istring "undelete")    <|> 
    try (DMLOpTok KW_Merge     <=: istring "merge"   )    <|>                                     
    try (KeywordTok KW_As        <=: istring "as")          <|>    
    try (KeywordTok KW_For       <=: istring "for")         <|> 
    try (KeywordTok KW_Do        <=: istring "do")          <|> 
    try (KeywordTok KW_If        <=: istring "if")          <|> 
    try (LiteralTok NullTok      <=: nullLiteral)           <|> 


    -- SOQL extra operator 
    try (SOQLExtraOpTok Op_Like      <=: istring "like"    )    <|>       
    try (SOQLExtraOpTok Op_Excludes  <=: istring "excludes")    <|>   
    try (SOQLExtraOpTok Op_Includes  <=: istring "includes")    <|>  
    try (SOQLExtraOpTok Op_In        <=: istring "in"      )    <|>
    try (SOQLExtraOpTok Op_Not       <=: istring "not"     )    <|>

    -- Operations & Symbols
    try (OperatorTok Op_Equals              <=: string "==")            <|>
    try (OperatorTok Op_LThanE              <=: string "<=")            <|>
    try (OperatorTok Op_GThanE              <=: string ">=")            <|>
    try (SOQLExtraOpTok Op_NotEq               <=: notEqual   )            <|>
    try (OperatorTok Op_AAnd                <=: string "&&")            <|>
    try (OperatorTok Op_OOr                 <=: string "||")            <|>
    try (OperatorTok Op_PPlus               <=: string "++")            <|>
    try (OperatorTok Op_MMinus              <=: string "--")            <|>
    try (OperatorTok Op_LShift              <=: string "<<")            <|>
    try (OperatorTok Op_PlusE               <=: string "+=")            <|>
    try (OperatorTok Op_MinusE              <=: string "-=")            <|>
    try (OperatorTok Op_StarE               <=: string "*=")            <|>
    try (OperatorTok Op_SlashE              <=: string "/=")            <|>
    try (OperatorTok Op_AndE                <=: string "&=")            <|>
    try (OperatorTok Op_OrE                 <=: string "|=")            <|>
    try (OperatorTok Op_CaretE              <=: string "^=")            <|>
    try (OperatorTok Op_PercentE            <=: string "%=")            <|>
    try (OperatorTok Op_LShiftE             <=: string "<<=")           <|>
    try (OperatorTok Op_RShiftE             <=: string ">>=")           <|>
    try (OperatorTok Op_RRShiftE            <=: string ">>>=")          <|>
    try (SymbolTok OpenParen              <=: char '(')               <|>
    try (SymbolTok CloseParen             <=: char ')')               <|>
    try (SymbolTok OpenSquare             <=: char '[')               <|>
    try (SymbolTok CloseSquare            <=: char ']')               <|>
    try (SymbolTok OpenCurly              <=: char '{')               <|>
    try (SymbolTok CloseCurly             <=: char '}')               <|>
    try (SymbolTok SemiColon              <=: char ';')               <|>
    try (SymbolTok Comma                  <=: char ',')               <|>
    try (OperatorTok Op_AtSign              <=: char '@')               <|>
    try (OperatorTok Op_Plus                <=: char '+')               <|>
    try (OperatorTok Op_Minus               <=: char '-')               <|>
    try (OperatorTok Op_Star                <=: char '*')               <|>
    try (OperatorTok Op_Slash               <=: char '/')               <|>
    try (OperatorTok Op_And                 <=: andOperator)            <|>
    try (OperatorTok Op_Or                  <=: orOperator)             <|>
    try (OperatorTok Op_Caret               <=: char '^')               <|>
    try (OperatorTok Op_Percent             <=: char '%')               <|>
    try (OperatorTok Op_Equal               <=: char '=')               <|> 
    try (OperatorTok Op_GThan               <=: char '>')               <|>
    try (OperatorTok Op_LThan               <=: char '<')               <|>
    try (OperatorTok Op_Bang                <=: char '!')               <|>
    try (OperatorTok Op_Tilde               <=: char '~')               <|>
    try (OperatorTok Op_Query               <=: char '?')               <|>
    try (OperatorTok Op_Colon               <=: char ':')                                                          
                         
    where 
        orOperator = (singleton <$> char '|') <|> istring "or" 
        andOperator = (singleton <$> char '&') <|> istring "and" 
        notEqual = (string "<>" <|> string "!=")
        period   = (char '.' <* notFollowedBy digit)
        strNDate fnName = istring fnName *> colon *> digits
        nullLiteral = (istring "nulls" <|> istring "null")
        colon = apexLexer.colon
        identTok = (IdentTok <<< IdentT) <<=: identifier

apexLexer :: TokenParser
apexLexer = makeTokenParser apexLanguage

apexLanguage :: LanguageDef
apexLanguage = do 
    let 
        (LanguageDef apex) = javaStyle 
        apexStyleDef = LanguageDef $ apex
                        { reservedNames = apexReservedNames
                        , reservedOpNames = apexReservedOpNames
                        } 
    apexStyleDef

apexReservedNames = 
    [ "override","object", "time" ,"date" ,"datetime" ,"when" ,"abstract" ,"integer", "null", "nulls", "update", "insert"
    , "assert" ,"boolean" ,"break" ,"blob" ,"case" ,"catch" ,"class" ,"const" ,"continue" , "double" ,"do" ,"else" 
    , "enum" ,"extends" ,"final" ,"finally" ,"decimal" ,"for" ,"if" ,"implements" ,"import" ,"instanceof" ,"interface" ,"long" 
    , "new" ,"private" ,"protected" ,"public" ,"return" ,"static" ,"super" ,"switch" ,"this" ,"transient" ,"try" ,"void" ,"while" 
    , "virtual", "global", "string", "in", "not", "merge", "upsert", "delete", "undelete", "as", "or", "not", "select"
    , "from", "where", "using", "asc", "desc", "limit", "and", "then", "end", "having"
    ]

apexReservedOpNames =
    [ "=" , ">" , "<" , "!" , "~" , "?" , ":" , "==" , "===" , "<=" , ">=" , "!=" , "!==" , "&&" , "||" , "++" , "--" , "+" 
    , "-" , "*" , "/" , "&" , "|" , "^" , "%" , "<<" , ">>" , ">>>" , "+=" , "-=" , "*=" , "/=" , "&=" , "|=" , "^=" , "%=" 
    , "<<=" , ">>=" , ">>>=" , "@" 
    ]


intTok         = (LiteralTok <<< IntegerTok)  <<=: integerLiteral
doubleTok      = (LiteralTok <<< DoubleTok)   <<=: doubleLiteral
longTok        = (LiteralTok <<< LongTok)     <<=: longLiteral
stringTok      = (LiteralTok <<< StringTok)   <<=: stringLiteral
boolTok        = (LiteralTok <<< BoolTok)     <<=: boolLiteral
opTok          = (OperatorTok <<< OpTok )     <<=: opLiteral
lParenTok      = SymbolTok OpenParen   <=: char '('
rParenTok      = SymbolTok CloseParen  <=: char ')'
lSquareTok     = SymbolTok OpenSquare  <=: char '['
rSquareTok     = SymbolTok CloseSquare <=: char ']'
lBraceTok      = SymbolTok OpenCurly   <=: char '{'
rBraceTok      = SymbolTok CloseCurly  <=: char '}'
semiColonTok   = SymbolTok SemiColon   <=: char ';'
atTok          = OperatorTok Op_AtSign <=: char '@'
commTok        = SymbolTok Comma       <=: char ','
periodTok      = SymbolTok Period      <=: (char '.' <* notFollowedBy digit)

keywordTable = HS.fromArray apexReservedNames
operatorTable = HS.fromArray apexReservedOpNames
isKeyword = flip HS.member keywordTable
isOperator = flip HS.member operatorTable

testSOQL = do
    _ <- string "["
    select <- istring "select"
    rest <- fromCharArray <$> (Array.many $ satisfy \c -> c /= ']')
    pure $ select <> rest

dot :: P String 
dot = apexLexer.dot 

identifier :: P String
identifier = apexLexer.identifier
    -- arrChar <- Array.cons <$> javaLetter <*> Array.many (try alphaNum <|> oneOf ['_', '$'])
    -- pure $ fromCharArray arrChar

-- parses a reserved name
reserved :: String -> P Unit
reserved = apexLexer.reserved    

 -- parses an operator
reservedOp :: String -> P Unit
reservedOp = apexLexer.reservedOp 

-- parses surrounding parenthesis:
parens :: forall t. P t -> P t
parens = apexLexer.parens      

brackets :: forall t. P t -> P t
brackets = apexLexer.brackets      

braces :: forall t. P t -> P t
braces = apexLexer.braces   

-- parses a semicolon
semi :: P String
semi = apexLexer.semi    

javaLetter :: P Char
javaLetter = satisfy (\c -> isAlpha c || c == '$' || c == '_')

 -- parses whitespace
whiteSpace :: P Unit
whiteSpace = apexLexer.whiteSpace 

-- Apex does not support binary, octal, or hex based on the link below 
-- https://salesforce.stackexchange.com/questions/151169/how-do-i-assign-values-to-properties-using-hexadecimal-notation-using-apex
-- It seem like a Encoding Util class must be used for hex convertion only
-- https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_restful_encodingUtil.htm
integerLiteral :: P Int 
integerLiteral = do 
    x <- decimalIntegerLiteral  <* notFollowedBy (char '.')
    pure $ x

doubleLiteral :: P Number 
doubleLiteral = apexLexer.float

longLiteral :: P BigInt.BigInt 
longLiteral = do 
    i <- zero <|> digitsStr
    _ <- integerTypeSuffix
    maybe (fail "Could not read long integer") pure $ BigInt.fromString i 
    where 
        zero = char '0' *> pure "0"

stringLiteral :: P String
stringLiteral = do 
    let stringCharacter = satisfy (\c -> c /= '\'' && c /= '\\') 
    fromCharArray <$> (char '\'' *> Array.many (stringCharacter <|> escapeChar) <* char '\'')

decimalIntegerLiteral :: P Int 
decimalIntegerLiteral = do 
    i <- decimalNumeral
    _ <- optional integerTypeSuffix
    pure i

boolLiteral :: P Boolean 
boolLiteral = readBool <$> (choice <<< map string) ["true", "false"]
    where 
        readBool "true"  = true
        readBool _       = false
    
opLiteral :: P String
-- opLiteral = (choice <<< map string) javaReservedOpNames
opLiteral = do
    let (LanguageDef apexLang) = apexLanguage
    ohead <- apexLang.opStart
    obody <- Array.many $ apexLang.opLetter
    let o = fromCharArray $ Array.cons ohead obody
    if isOperator o 
    then pure o
    else fail ("Unknown operator" <> o)

funcLiteral :: String -> P Unit 
funcLiteral s = do 
    _ <- istring s
    _ <- lookAhead $ char '('
    pure unit
    
-- https://developer.salesforce.com/docs/atlas.en-us.soql_sosl.meta/soql_sosl/sforce_api_calls_soql_select_dateformats.htm
-- Format: YYYY-MM-DD
dateLiteral :: P String 
dateLiteral = do 
    year  <- sequence $ [digit, digit, digit, digit]
    dash1 <- sequence $ [char '-']
    month <- sequence $ [digit, digit]
    dash2 <- sequence $ [char '-']
    day   <- sequence $ [digit, digit] 
    
    pure $ fromCharArray $ Array.concat [year, dash1, month, dash2, day]

-- https://developer.salesforce.com/docs/atlas.en-us.soql_sosl.meta/soql_sosl/sforce_api_calls_soql_select_dateformats.htm
-- Format: YYYY-MM-DDThh:mm:ssZ | YYYY-MM-DDThh:mm:ss+hh:mm | YYYY-MM-DDThh:mm:ss-hh:mm 
datetimeLiteral :: P String 
datetimeLiteral = do 
    date       <- dateLiteral 
    t          <- sequence $ [char 'T']
    hour       <- sequence $ [digit, digit]
    c1         <- sequence $ [char ':']
    mins       <- sequence $ [digit, digit]
    c2         <- sequence $ [char ':']
    secs       <- sequence $ [digit, digit]
    zuluOffset <- sequence [char 'Z'] <|>  sequence [char '+'] <|>  sequence [char '-']
    ofhour     <- option [] $ sequence $ [digit, digit]
    c3         <- option [] $ sequence $ [char ':']
    ofmins     <- option [] $ sequence $ [digit, digit]

    pure $ date <> (fromCharArray $ Array.concat [t, hour, c1, mins, c2, secs, zuluOffset, ofhour, c3, ofmins])

escapeChar :: P Char
escapeChar = choice (map parseEsc escMap)
        where 

            parseEsc :: Tuple Char Char -> P Char
            parseEsc (Tuple c code) = try (char '\\' *>  char c $> code)

            -- escape code tables
            escMap :: Array (Tuple Char Char)
            escMap = Array.zip [   'a',   'b',   'f',  'n',  'r',  't',   'v', '\\', '\"', '\'' ]
                               [ '\x7', '\x8', '\xC', '\n', '\r', '\t', '\xB', '\\', '\"', '\'' ]

decimalNumeral :: P Int 
decimalNumeral = zero <|> digits
    where 
        zero      = char '0' *> pure 0

digits :: P Int 
digits = do 
    ds <- digitsStr 
    maybe (fail "expected digit") pure $ Int.fromString ds 

digitsStr :: P String 
digitsStr = many1 digit >>= (pure <<< fromCharArray <<< toUnfoldable) 

integerTypeSuffix :: P Char 
integerTypeSuffix = char 'l' <|> char 'L'
