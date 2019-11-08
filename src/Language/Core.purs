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
import Data.List.Lazy (List, toUnfoldable, fromFoldable, many)
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
    try (KW_SOQL                <<=: testSOQL)                <|>
    try (DatetimeTok            <<=: datetimeLiteral)         <|>
    try (DateTok                <<=: dateLiteral)             <|>
    try (LongTok                <<=: longLiteral)              <|>
    try (DoubleTok              <<=: doubleLiteral)            <|>
    try (IntegerTok             <<=: integerLiteral)           <|>
    try (StringTok              <<=: stringLiteral)            <|>
    try (BoolTok                <<=: boolLiteral)              <|>
    try (Period                 <=:  period     )              <|>
    try (KW_Override            <=: istring "override" )       <|>
    try (KW_With_Share          <=: istring "with sharing" )   <|>
    try (KW_Without_Share       <=: istring "without sharing") <|> 
    try (KW_Inherit_Share       <=: istring "inherit sharing") <|>
    try (KW_Object              <=: istring "object" )         <|>
    try (KW_Time                <=: istring "time")            <|> 
    try (KW_Date                <=: istring "date")            <|> 
    try (KW_Datetime            <=: istring "datetime")        <|>
    try (KW_When                <=: istring "when")            <|> 
    try (KW_Abstract            <=: istring "abstract")        <|> 
    try (KW_Integer             <=: istring "integer")         <|> 
    try (KW_String              <=: istring "string")          <|>
    try (KW_Assert              <=: istring "assert")          <|> 
    try (KW_Boolean             <=: istring "boolean")         <|> 
    try (KW_Break               <=: istring "break")           <|> 
    try (KW_Blob                <=: istring "blob")            <|> 
    try (KW_Case                <=: istring "case")            <|> 
    try (KW_Catch               <=: istring "catch")           <|> 
    try (KW_Class               <=: istring "class")           <|> 
    try (KW_Const               <=: istring "const")           <|> 
    try (KW_Continue            <=: istring "continue")        <|> 
    try (KW_WhenElse            <=: istring "when else")       <|> 
    try (KW_Double              <=: istring "double")          <|> 

    try (KW_Else                <=: istring "else")            <|> 
    try (KW_Enum                <=: istring "enum")            <|> 
    try (KW_Extends             <=: istring "extends")         <|> 
    try (KW_Final               <=: istring "final")           <|> 
    try (KW_Finally             <=: istring "finally")         <|> 
    try (KW_Decimal             <=: istring "decimal")         <|> 
    try (KW_Implements          <=: istring "implements")      <|> 
    try (KW_Import              <=: istring "import")          <|> 
    try (KW_Instanceof          <=: istring "instanceof")      <|> 
    try (KW_Interface           <=: istring "interface")       <|> 
    try (KW_Long                <=: istring "long")            <|> 
    try (KW_New                 <=: istring "new")             <|> 
    try (KW_Private             <=: istring "private")         <|> 
    try (KW_Virtual             <=: istring "virtual")         <|> 
    try (KW_Global              <=: istring "global")          <|> 
    try (KW_Protected           <=: istring "protected")       <|> 
    try (KW_Public              <=: istring "public")          <|> 
    try (KW_Return              <=: istring "return")          <|> 
    try (KW_Static              <=: istring "static")          <|> 
    try (KW_Super               <=: istring "super")           <|> 
    try (KW_Switch              <=: istring "switch on")       <|> 
    try (KW_This                <=: istring "this")            <|> 
    try (KW_Transient           <=: istring "transient")       <|> 
    try (KW_Try                 <=: istring "try")             <|> 
    try (KW_Void                <=: istring "void")            <|> 
    try (KW_While               <=: istring "while")           <|> 
    try (KW_OrderBy   <=: istring "order by")    <|>    
    try (KW_Desc      <=: istring "desc"    )    <|>  
    try (KW_Then      <=: istring "then"    )    <|>  
    try (KW_End       <=: istring "end"    )     <|>  
    try (KW_Asc       <=: istring "asc"     )    <|>  

    try (KW_NullFirst <=: istring "nulls first")  <|>   
    try (KW_From      <=: istring "from"    )    <|>
    try (KW_GroupByCube   <=: istring "group by cube"  )  <|>     
    try (KW_GroupByRollup <=: istring "group by rollup")  <|>  
    try (KW_GroupBy   <=: istring "group by")    <|>   
    try (KW_Having    <=: istring "having"  )    <|>   
    try (KW_NullLast  <=: istring "nulls last")  <|>    
    try (KW_Limit     <=: istring "limit"   )    <|>    
    try (KW_Select    <=: istring "select"  )    <|>   
    try (KW_Using     <=: istring "using"   )    <|>   
    try (KW_Where     <=: istring "where"   )    <|>    

    -- DML Keywords
    try (KW_Update    <=: istring "update"  )    <|> 
    try (KW_Insert    <=: istring "insert"  )    <|> 
    try (KW_Upsert    <=: istring "upsert"  )    <|> 
    try (KW_Delete    <=: istring "delete"  )    <|> 
    try (KW_Undelete  <=: istring "undelete")    <|> 
    try (KW_Merge     <=: istring "merge"   )    <|> 

    -- SOQL Date Related Token 
    try (Next_n_days             <<=: strNDate "next_n_days" ) <|> 
    try (Last_n_days             <<=: strNDate "last_n_days" ) <|> 
    try (N_days_ago              <<=: strNDate "n_days_ago" ) <|> 
    try (Next_n_weeks            <<=: strNDate "next_n_weeks" ) <|> 
    try (Last_n_weeks            <<=: strNDate "last_n_weeks" ) <|> 
    try (N_weeks_ago             <<=: strNDate "n_weeks_ago" ) <|>
    try (Next_n_months           <<=: strNDate "next_n_months" ) <|>
    try (Last_n_months           <<=: strNDate "last_n_months" ) <|> 
    try (N_months_ago            <<=: strNDate "n_months_ago" ) <|> 
    try (Next_n_quarters         <<=: strNDate "next_n_quarters" ) <|> 
    try (Last_n_quarters         <<=: strNDate "last_n_quarters" ) <|> 
    try (N_quarters_ago          <<=: strNDate "n_quarters_ago" ) <|> 
    try (Next_n_years            <<=: strNDate "next_n_years" ) <|> 
    try (Last_n_years            <<=: strNDate "last_n_years" ) <|> 
    try (N_years_ago             <<=: strNDate "n_years_ago" ) <|> 
    try (Next_n_fiscal_quarters  <<=: strNDate "next_n_fiscal_quarters" ) <|> 
    try (Last_n_fiscal_quarters  <<=: strNDate "last_n_fiscal_quarters" ) <|> 
    try (N_fiscal_quarters_ago   <<=: strNDate "n_fiscal_quarters_ago" ) <|> 
    try (Next_n_fiscal_years     <<=: strNDate "next_n_fiscal_years" ) <|> 
    try (Last_n_fiscal_years     <<=: strNDate "last_n_fiscal_years" ) <|> 
    try (N_fiscal_years_ago      <<=: strNDate "n_fiscal_years_ago" ) <|>
    try (Yesterday            <=: istring "yesterday" ) <|> 
    try (Today                <=: istring "today" ) <|> 
    try (Tomorrow             <=: istring "tomorrow" ) <|> 
    try (Last_week            <=: istring "last_week" ) <|> 
    try (This_week            <=: istring "this_week" ) <|> 
    try (Next_week            <=: istring "next_week" ) <|> 
    try (Last_month           <=: istring "last_month" ) <|> 
    try (This_month           <=: istring "this_month" ) <|> 
    try (Next_month           <=: istring "next_month" ) <|> 
    try (Last_90_days         <=: istring "last_90_days" ) <|> 
    try (Next_90_days         <=: istring "next_90_days" ) <|> 
    try (This_quarter         <=: istring "this_quarter" ) <|> 
    try (Last_quarter         <=: istring "last_quarter" ) <|> 
    try (Next_quarter         <=: istring "next_quarter" ) <|> 
    try (This_year            <=: istring "this_year" ) <|> 
    try (Last_year            <=: istring "last_year" ) <|> 
    try (Next_year            <=: istring "next_year" ) <|> 
    try (This_fiscal_quarter  <=: istring "this_fiscal_quarter" ) <|> 
    try (Last_fiscal_quarter  <=: istring "last_fiscal_quarter" ) <|> 
    try (Next_fiscal_quarter  <=: istring "next_fiscal_quarter" ) <|> 
    try (This_fiscal_year     <=: istring "this_fiscal_year" ) <|> 
    try (Last_fiscal_year     <=: istring "last_fiscal_year" ) <|> 
    try (Next_fiscal_year     <=: istring "next_fiscal_year" ) <|>
    try (KW_Format               <=: funcLiteral "format" ) <|> 
    try (KW_Tolabel              <=: funcLiteral "tolabel" ) <|> 
    try (KW_Convert_time_zone    <=: funcLiteral "converttimezone" ) <|> 
    try (KW_Convert_currency     <=: funcLiteral "convertcurrency" ) <|> 
    try (KW_Grouping             <=: funcLiteral "grouping" ) <|> 
    try (KW_Distance             <=: funcLiteral "distance" ) <|> 
    try (KW_Geolocation          <=: funcLiteral "geolocation" ) <|>
    try (KW_Avg                  <=: funcLiteral "avg" ) <|> 
    try (KW_Count                <=: funcLiteral "count" ) <|> 
    try (KW_Count_distinct       <=: funcLiteral "count_distinct" ) <|> 
    try (KW_Min                  <=: funcLiteral "min" ) <|> 
    try (KW_Max                  <=: funcLiteral "max" ) <|> 
    try (KW_Sum                  <=: funcLiteral "sum" ) <|>
    try (KW_Calendar_month       <=: funcLiteral "calendar_month" ) <|> 
    try (KW_Calendar_quarter     <=: funcLiteral "calendar_quarter" ) <|> 
    try (KW_Calendar_year        <=: funcLiteral "calendar_year" ) <|> 
    try (KW_Day_in_month         <=: funcLiteral "day_in_month" ) <|> 
    try (KW_Day_in_week          <=: funcLiteral "day_in_week" ) <|> 
    try (KW_Day_in_year          <=: funcLiteral "day_in_year" ) <|> 
    try (KW_Day_only             <=: funcLiteral "day_only" ) <|> 
    try (KW_Fiscal_month         <=: funcLiteral "fiscal_month" ) <|> 
    try (KW_Fiscal_quarter       <=: funcLiteral "fiscal_quarter" ) <|> 
    try (KW_Fiscal_year          <=: funcLiteral "fiscal_year" ) <|> 
    try (KW_Hour_in_day          <=: funcLiteral "hour_in_day" ) <|> 
    try (KW_Week_in_month        <=: funcLiteral "week_in_month" ) <|> 
    try (KW_Week_in_year         <=: funcLiteral "week_in_year" )  <|>
    
    try (KW_As                  <=: istring "as")              <|>    
    try (KW_For                 <=: istring "for")             <|> 
    try (KW_Do                  <=: istring "do")              <|> 
    try (KW_If                  <=: istring "if")              <|> 
    try (NullTok                <=: nullLiteral)               <|> 


    -- SOQL extra operator 
    try (Op_Like      <=: istring "like"    )    <|>       
    try (Op_Excludes  <=: istring "excludes")    <|>   
    try (Op_Includes  <=: istring "includes")    <|>  
    try (Op_NotIn     <=: istring "not in"  )    <|>

    identTok                                     <|>

    try (Op_In        <=: istring "in"      )    <|>
    try (Op_Not       <=: istring "not"     )    <|>

    -- Operations & Symbols
    try (Op_Equals              <=: string "==")            <|>
    try (Op_LThanE              <=: string "<=")            <|>
    try (Op_GThanE              <=: string ">=")            <|>
    try (Op_NotEq               <=: notEqual   )            <|>
    try (Op_AAnd                <=: string "&&")            <|>
    try (Op_OOr                 <=: string "||")            <|>
    try (Op_PPlus               <=: string "++")            <|>
    try (Op_MMinus              <=: string "--")            <|>
    try (Op_LShift              <=: string "<<")            <|>
    try (Op_PlusE               <=: string "+=")            <|>
    try (Op_MinusE              <=: string "-=")            <|>
    try (Op_StarE               <=: string "*=")            <|>
    try (Op_SlashE              <=: string "/=")            <|>
    try (Op_AndE                <=: string "&=")            <|>
    try (Op_OrE                 <=: string "|=")            <|>
    try (Op_CaretE              <=: string "^=")            <|>
    try (Op_PercentE            <=: string "%=")            <|>
    try (Op_LShiftE             <=: string "<<=")           <|>
    try (Op_RShiftE             <=: string ">>=")           <|>
    try (Op_RRShiftE            <=: string ">>>=")          <|>
    try (OpenParen              <=: char '(')               <|>
    try (CloseParen             <=: char ')')               <|>
    try (OpenSquare             <=: char '[')               <|>
    try (CloseSquare            <=: char ']')               <|>
    try (OpenCurly              <=: char '{')               <|>
    try (CloseCurly             <=: char '}')               <|>
    try (Op_AtSign              <=: char '@')               <|>
    try (SemiColon              <=: char ';')               <|>
    try (Comma                  <=: char ',')               <|>
    try (Period                 <=: period    )                <|>
    try (Op_Plus                <=: char '+')               <|>
    try (Op_Minus               <=: char '-')               <|>
    try (Op_Star                <=: char '*')               <|>
    try (Op_Slash               <=: char '/')               <|>
    try (Op_And                 <=: andOperator)            <|>
    try (Op_Or                  <=: orOperator)              <|>
    try (Op_Caret               <=: char '^')               <|>
    try (Op_Percent             <=: char '%')               <|>
    try (Op_Equal               <=: char '=')               <|> 
    try (Op_GThan               <=: char '>')               <|>
    try (Op_LThan               <=: char '<')               <|>
    try (Op_Bang                <=: char '!')               <|>
    try (Op_Tilde               <=: char '~')               <|>
    try (Op_Query               <=: char '?')               <|>
    try (Op_Colon               <=: char ':')                                                      
    where 
        orOperator = (singleton <$> char '|') <|> istring "or" 
        andOperator = (singleton <$> char '&') <|> istring "and" 
        notEqual = (string "<>" <|> string "!=")
        period   = (char '.' <* notFollowedBy digit)
        strNDate fnName = istring fnName *> colon *> digits
        nullLiteral = (istring "nulls" <|> istring "null")
        colon = javaLexer.colon
        identTok = IdentTok <<=: identifier

javaLexer :: TokenParser
javaLexer = makeTokenParser javaLanguage

javaLanguage :: LanguageDef
javaLanguage = do 
    let 
        (LanguageDef java) = javaStyle 
        javaStyleDef = LanguageDef $ java
                        { reservedNames = javaReservedNames
                        , reservedOpNames = javaReservedOpNames
                        } 
    javaStyleDef

javaReservedNames = 
    [ "override","object", "time" ,"date" ,"datetime" ,"when" ,"abstract" ,"integer" 
    , "assert" ,"boolean" ,"break" ,"blob" ,"case" ,"catch" ,"class" ,"const" ,"continue" ,"when" ,"double" ,"do" ,"else" 
    , "enum" ,"extends" ,"final" ,"finally" ,"decimal" ,"for" ,"if" ,"implements" ,"import" ,"instanceof" ,"interface" ,"long" 
    , "new" ,"private" ,"protected" ,"public" ,"return" ,"static" ,"super" ,"switch" ,"this" ,"transient" ,"try" ,"void" ,"while" 
    , "virtual", "global", "string", "in", "not"
    ]

    

javaReservedOpNames =
    [ "=" , ">" , "<" , "!" , "~" , "?" , ":" , "==" , "===" , "<=" , ">=" , "!=" , "!==" , "&&" , "||" , "++" , "--" , "+" 
    , "-" , "*" , "/" , "&" , "|" , "^" , "%" , "<<" , ">>" , ">>>" , "+=" , "-=" , "*=" , "/=" , "&=" , "|=" , "^=" , "%=" 
    , "<<=" , ">>=" , ">>>=" , "@" 
    ]


intTok         = IntegerTok  <<=: integerLiteral
doubleTok      = DoubleTok   <<=: doubleLiteral
longTok        = LongTok     <<=: longLiteral
stringTok      = StringTok   <<=: stringLiteral
boolTok        = BoolTok     <<=: boolLiteral
opTok          = OpTok       <<=: opLiteral
lParenTok      = OpenParen   <=: char '('
rParenTok      = CloseParen  <=: char ')'
lSquareTok     = OpenSquare  <=: char '['
rSquareTok     = CloseSquare <=: char ']'
lBraceTok      = OpenCurly   <=: char '{'
rBraceTok      = CloseCurly  <=: char '}'
semiColonTok   = SemiColon   <=: char ';'
atTok          = Op_AtSign   <=: char '@'
commTok        = Comma       <=: char ','
periodTok      = Period      <=: (char '.' <* notFollowedBy digit)

keywordTable = HS.fromArray javaReservedNames
operatorTable = HS.fromArray javaReservedOpNames
isKeyword = flip HS.member keywordTable
isOperator = flip HS.member operatorTable

testSOQL = do
    _ <- string "["
    select <- istring "select"
    rest <- fromCharArray <$> (Array.many $ satisfy \c -> c /= ']')
    pure $ select <> rest

dot :: P String 
dot = javaLexer.dot 

identifier :: P String
identifier = javaLexer.identifier
    -- arrChar <- Array.cons <$> javaLetter <*> Array.many (try alphaNum <|> oneOf ['_', '$'])
    -- pure $ fromCharArray arrChar

-- parses a reserved name
reserved :: String -> P Unit
reserved = javaLexer.reserved    

 -- parses an operator
reservedOp :: String -> P Unit
reservedOp = javaLexer.reservedOp 

-- parses surrounding parenthesis:
parens :: forall t. P t -> P t
parens = javaLexer.parens      

brackets :: forall t. P t -> P t
brackets = javaLexer.brackets      

braces :: forall t. P t -> P t
braces = javaLexer.braces   

-- parses a semicolon
semi :: P String
semi = javaLexer.semi    

javaLetter :: P Char
javaLetter = satisfy (\c -> isAlpha c || c == '$' || c == '_')

 -- parses whitespace
whiteSpace :: P Unit
whiteSpace = javaLexer.whiteSpace 

-- Apex does not support binary, octal, or hex based on the link below 
-- https://salesforce.stackexchange.com/questions/151169/how-do-i-assign-values-to-properties-using-hexadecimal-notation-using-apex
-- It seem like a Encoding Util class must be used for hex convertion only
-- https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_restful_encodingUtil.htm
integerLiteral :: P Int 
integerLiteral = do 
    x <- decimalIntegerLiteral  <* notFollowedBy (char '.')
    pure $ x

doubleLiteral :: P Number 
doubleLiteral = javaLexer.float

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
    let (LanguageDef javaLang) = javaLanguage
    ohead <- javaLang.opStart
    obody <- Array.many $ javaLang.opLetter
    let o = fromCharArray $ Array.cons ohead obody
    if isOperator o 
    then pure o
    else fail ("Unknown operator" <> o)

funcLiteral :: String -> P Unit 
funcLiteral s = do 
    s <- istring s
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
