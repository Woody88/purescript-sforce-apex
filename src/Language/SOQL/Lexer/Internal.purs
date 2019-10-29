module Language.SOQL.Lexer.Internal where

import Prelude 
import Control.Apply ((<*), (*>))
import Control.Alternative ((<|>))
import Data.BigInt as BigInt
import Data.Array as Array
import Data.List (toUnfoldable)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Language.SOQL.Lexer.Types (P, Token(..))
import Language.SOQL.Lexer.Utils (many1)
import Language.Types (L)
import Language.SOQL.Lexer.Utils ((<<=:), (<=:), istring)
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.Combinators (try, notFollowedBy, optional, choice)
import Text.Parsing.Parser.String (string, char, satisfy)
import Text.Parsing.Parser.Token 
import Unsafe.Coerce (unsafeCoerce)

readToken :: P (L Token)
readToken = 
    try (LongTok                 <<=: longLiteral)              <|>
    try (DoubleTok               <<=: doubleLiteral)            <|>
    try (IntegerTok              <<=: integerLiteral)           <|>
    try (StringTok               <<=: stringLiteral)            <|>
    try (BoolTok                 <<=: boolLiteral)              <|>
    try (NullTok                 <=:  nullLiteral   )           <|>   
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
    try (Format               <=: istring "format" ) <|> 
    try (Tolabel              <=: istring "tolabel" ) <|> 
    try (Convert_time_zone    <=: istring "convert_time_zone" ) <|> 
    try (Convert_currency     <=: istring "convert_currency" ) <|> 
    try (Grouping             <=: istring "grouping" ) <|> 
    try (Distance             <=: istring "distance" ) <|> 
    try (Geolocation          <=: istring "geolocation" ) <|>
    try (Avg                  <=: istring "avg" ) <|> 
    try (Count                <=: istring "count" ) <|> 
    try (Count_distinct       <=: istring "count_distinct" ) <|> 
    try (Min                  <=: istring "min" ) <|> 
    try (Max                  <=: istring "max" ) <|> 
    try (Sum                  <=: istring "sum" ) <|>
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
    try (Above            <=:    istring "above"          ) <|>                  
    try (Above_or_below   <=:    istring "above_or_below" ) <|>         
    try (At               <=:    istring "at"             ) <|>                     
    try (Below            <=:    istring "below"          ) <|>                  
    try (Category         <=:    istring "category"       ) <|>               
    try (Data             <=:    istring "data"           ) <|>                   
    try (End              <=:    istring "end"            ) <|>                    
    try (Offset           <=:    istring "offset"         ) <|>                 
    try (Group            <=:    istring "group"          ) <|>                 
    try (Order            <=:    istring "order"          ) <|>                  
    try (Reference        <=:    istring "reference"      ) <|>              
    try (Scope            <=:    istring "scope"          ) <|>                  
    try (Tracking         <=:    istring "tracking"       ) <|>               
    try (Then             <=:    istring "then"           ) <|>                   
    try (Typeof           <=:    istring "typeof"         ) <|>                 
    try (View             <=:    istring "view"           ) <|>                   
    try (Viewstat         <=:    istring "viewstat"       ) <|>
    try (When             <=:    istring "when"           ) <|>
    try (KW_Asc       <=: istring "asc"     )    <|>  
    try (KW_As        <=: istring "as"      )    <|>    
    try (KW_By        <=: istring "by"      )    <|>   
    try (KW_Cube      <=: istring "cube"    )    <|>   
    try (KW_Desc      <=: istring "desc"    )    <|>   
    try (KW_Else      <=: istring "else"    )    <|>   
    try (KW_First     <=: istring "first"   )    <|>   
    try (KW_From      <=: istring "from"    )    <|>   
    try (KW_Group     <=: istring "group"   )    <|>   
    try (KW_Having    <=: istring "having"  )    <|>   
    try (KW_Last      <=: istring "last"    )    <|>    
    try (KW_Limit     <=: istring "limit"   )    <|>   
    try (KW_Rollup    <=: istring "rollup"  )    <|>   
    try (KW_Select    <=: istring "select"  )    <|>   
    try (KW_Using     <=: istring "using"   )    <|>   
    try (KW_Where     <=: istring "where"   )    <|>   
    try (KW_With      <=: istring "with"    )    <|>   
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
digits = unsafeCoerce <$> digitsStr 
    

digitsStr :: P String 
digitsStr = many1 digit >>= (pure <<< fromCharArray <<< toUnfoldable) 

integerTypeSuffix :: P Char 
integerTypeSuffix = char 'l' <|> char 'L'
