module Language.SOQL.Lexer.Internal where

import Prelude
import Text.Parsing.Parser.Token

import Control.Alternative ((<|>))
import Control.Apply ((<*), (*>))
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Int as Int
import Data.List (toUnfoldable)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..))
import Language.SOQL.Lexer.Types (P, Token(..))
import Language.SOQL.Lexer.Utils ((<<=:), (<=:), istring)
import Language.SOQL.Lexer.Utils (many1)
import Language.Types (L)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (try, lookAhead, notFollowedBy, option, optional, choice)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String (string, char, satisfy)

readToken :: P (L Token)
readToken = 
    try (DatetimeTok             <<=: datetimeLiteral)          <|>
    try (DateTok                 <<=: dateLiteral)              <|>
    try (LongTok                 <<=: longLiteral)              <|>
    try (DoubleTok               <<=: doubleLiteral)            <|>
    try (IntegerTok              <<=: integerLiteral)           <|>
    try (StringTok               <<=: stringLiteral)            <|>
    try (BoolTok                 <<=: boolLiteral)              <|>
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
    try (Format               <=: funcLiteral "format" ) <|> 
    try (Tolabel              <=: funcLiteral "tolabel" ) <|> 
    try (Convert_time_zone    <=: funcLiteral "convert_time_zone" ) <|> 
    try (Convert_currency     <=: funcLiteral "convert_currency" ) <|> 
    try (Grouping             <=: funcLiteral "grouping" ) <|> 
    try (Distance             <=: funcLiteral "distance" ) <|> 
    try (Geolocation          <=: funcLiteral "geolocation" ) <|>
    try (Avg                  <=: funcLiteral "avg" ) <|> 
    try (Count                <=: funcLiteral "count" ) <|> 
    try (Count_distinct       <=: funcLiteral "count_distinct" ) <|> 
    try (Min                  <=: funcLiteral "min" ) <|> 
    try (Max                  <=: funcLiteral "max" ) <|> 
    try (Sum                  <=: funcLiteral "sum" ) <|>
    try (Calendar_month       <=: istring "calendar_month" ) <|> 
    try (Calendar_quarter     <=: istring "calendar_quarter" ) <|> 
    try (Calendar_year        <=: istring "calendar_year" ) <|> 
    try (Day_in_month         <=: istring "day_in_month" ) <|> 
    try (Day_in_week          <=: istring "day_in_week" ) <|> 
    try (Day_in_year          <=: istring "day_in_year" ) <|> 
    try (Day_only             <=: istring "day_only" ) <|> 
    try (Fiscal_month         <=: istring "fiscal_month" ) <|> 
    try (Fiscal_quarter       <=: istring "fiscal_quarter" ) <|> 
    try (Fiscal_year          <=: istring "fiscal_year" ) <|> 
    try (Hour_in_day          <=: istring "hour_in_day" ) <|> 
    try (Week_in_month        <=: istring "week_in_month" ) <|> 
    try (Week_in_year         <=: istring "week_in_year" )  <|>
    try (KW_Asc       <=: istring "asc"     )    <|>  
    try (KW_As        <=: istring "as"      )    <|>    
    try (KW_OrderBy   <=: istring "order by")    <|>   
    try (KW_Cube      <=: istring "cube"    )    <|>   
    try (KW_Desc      <=: istring "desc"    )    <|>   
    try (KW_Else      <=: istring "else"    )    <|>   
    try (KW_NullFirst <=: istring "nulls first")  <|>   
    try (KW_From      <=: istring "from"    )    <|>   
    try (KW_Group     <=: istring "group"   )    <|>   
    try (KW_Having    <=: istring "having"  )    <|>   
    try (KW_NullLast  <=: istring "nulls last")  <|>    
    try (KW_Limit     <=: istring "limit"   )    <|>   
    try (KW_Rollup    <=: istring "rollup"  )    <|>   
    try (KW_Select    <=: istring "select"  )    <|>   
    try (KW_Using     <=: istring "using"   )    <|>   
    try (KW_Where     <=: istring "where"   )    <|>    
    try (KW_For       <=: istring "for"     )    <|>   
    try (KW_Update    <=: istring "update"  )    <|> 
    try (Op_Like      <=: istring "like"    )    <|>  
    try (Op_And       <=: istring "and"     )    <|>      
    try (Op_Or        <=: istring "or"      )    <|>     
    try (Op_Excludes  <=: istring "excludes")    <|>   
    try (Op_Includes  <=: istring "includes")    <|>  
    try (Op_NotIn     <=: istring "not in"  )    <|>
    try (Op_In        <=: istring "in"      )    <|>
    try (Op_NotEq     <=: notEqual)       <|>
    try (Op_GThanE    <=: string ">=")    <|>
    try (Op_LThanE    <=: string "<=")    <|>
    try (Op_Not       <=: istring "not")  <|>    
    try (OpenParen    <=: char '(')       <|>
    try (CloseParen   <=: char ')')       <|>
    try (OpenSquare   <=: char '[')       <|>
    try (CloseSquare  <=: char ']')       <|>
    try (SemiColon    <=: char ';')       <|>
    try (Colon        <=: char ':')       <|>
    try (Comma        <=: char ',')       <|>
    try (Period       <=: period  )       <|>
    try (Plus         <=: char '+')       <|>
    try (Minus        <=: char '-')       <|>
    try (Asterisk     <=: char '*')       <|>
    try (Slash        <=: char '/')       <|>
    try (Op_Eq        <=: char '=')       <|> 
    try (Op_GThan     <=: char '>')       <|>
    try (Op_LThan     <=: char '<')       <|>
    try (NullTok      <=:  nullLiteral)   <|>   
    try (Ident        <<=: javaLexer.identifier )
    where 
        notEqual = (string "<>" <|> string "!=")
        period   = (char '.' <* notFollowedBy digit)
        strNDate fnName = istring fnName *> colon *> digits
        nullLiteral = (istring "nulls" <|> istring "null")
        colon = javaLexer.colon

javaLexer :: TokenParser
javaLexer = makeTokenParser javaLanguage

javaLanguage :: LanguageDef
javaLanguage = javaStyle

funcLiteral :: String -> P Unit 
funcLiteral s = do 
    s <- istring s
    _ <- lookAhead $ char '('
    pure unit
     
longLiteral :: P BigInt.BigInt 
longLiteral = do 
    i <- zero <|> digitsStr
    _ <- integerTypeSuffix
    maybe (fail "Could not read long integer") pure $ BigInt.fromString i 
    where 
        zero = char '0' *> pure "0"
            
integerLiteral :: P Int 
integerLiteral = do 
    x <- decimalIntegerLiteral  <* notFollowedBy (char '.')
    pure $ x

doubleLiteral :: P Number 
doubleLiteral = javaLexer.float

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
boolLiteral = readBool <$> (choice <<< map istring) ["true", "false"]
    where 
        readBool "true"  = true
        readBool _       = false

decimalNumeral :: P Int 
decimalNumeral = zero <|> digits
    where 
        zero      = char '0' *> pure 0

escapeChar :: P Char
escapeChar = choice (map parseEsc escMap)
    where 

        parseEsc :: Tuple Char Char -> P Char
        parseEsc (Tuple c code) = try (char '\\' *>  char c $> code)

        -- escape code tables
        escMap :: Array (Tuple Char Char)
        escMap = Array.zip [   'a',   'b',   'f',  'n',  'r',  't',   'v', '\\', '\"', '\'' ]
                           [ '\x7', '\x8', '\xC', '\n', '\r', '\t', '\xB', '\\', '\"', '\'' ]

digits :: P Int 
digits = do 
    ds <- digitsStr 
    maybe (fail "expected digit") pure $ Int.fromString ds 
    

digitsStr :: P String 
digitsStr = many1 digit >>= (pure <<< fromCharArray <<< toUnfoldable) 

integerTypeSuffix :: P Char 
integerTypeSuffix = char 'l' <|> char 'L'
