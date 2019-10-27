module Language.Apex.Lexer.Internal where 

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Char.Unicode (isAlpha, toLower, toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.Either (Either(..))
import Data.HashSet as HS
import Data.Int as Int
import Data.BigInt as BigInt
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (logShow, log)
import Text.Parsing.Parser (runParser, position, fail)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Language as PL
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser)
import Text.Parsing.Parser.Token as PT
import Language.Apex.Lexer.Types 
import Language.Apex.Lexer.Utils 
import Language.Types (L(..))
import Unsafe.Coerce (unsafeCoerce)


readToken :: P (L Token)
readToken = 
    PC.try identTok                                               <|>
    PC.try (LongTok                <<=: longLiteral)              <|>
    PC.try (DoubleTok              <<=: doubleLiteral)            <|>
    PC.try (IntegerTok             <<=: integerLiteral)           <|>
    PC.try (StringTok              <<=: stringLiteral)            <|>
    PC.try (BoolTok                <<=: boolLiteral)              <|>
    PC.try (Period                 <=:  period     )              <|>
    PC.try (KW_Override            <=: istring "override" )       <|>
    PC.try (KW_With_Share          <=: istring "with sharing" )   <|>
    PC.try (KW_Without_Share       <=: istring "without sharing") <|> 
    PC.try (KW_Inherit_Share       <=: istring "inherit sharing") <|>
    PC.try (KW_Object              <=: istring "object" )         <|>
    PC.try (KW_Time                <=: istring "time")            <|> 
    PC.try (KW_ID                  <=: istring "id")              <|>
    PC.try (KW_Date                <=: istring "date")            <|> 
    PC.try (KW_Datetime            <=: istring "datetime")        <|>
    PC.try (KW_When                <=: istring "when")            <|> 
    PC.try (KW_Abstract            <=: istring "abstract")        <|> 
    PC.try (KW_Integer             <=: istring "integer")         <|> 
    PC.try (KW_String              <=: istring "string")          <|>
    PC.try (KW_Assert              <=: istring "assert")          <|> 
    PC.try (KW_Boolean             <=: istring "boolean")         <|> 
    PC.try (KW_Break               <=: istring "break")           <|> 
    PC.try (KW_Blob                <=: istring "blob")            <|> 
    PC.try (KW_Case                <=: istring "case")            <|> 
    PC.try (KW_Catch               <=: istring "catch")           <|> 
    PC.try (KW_Class               <=: istring "class")           <|> 
    PC.try (KW_Const               <=: istring "const")           <|> 
    PC.try (KW_Continue            <=: istring "continue")        <|> 
    PC.try (KW_WhenElse            <=: istring "when else")       <|> 
    PC.try (KW_Double              <=: istring "double")          <|> 
    PC.try (KW_Do                  <=: istring "do")              <|> 
    PC.try (KW_Else                <=: istring "else")            <|> 
    PC.try (KW_Enum                <=: istring "enum")            <|> 
    PC.try (KW_Extends             <=: istring "extends")         <|> 
    PC.try (KW_Final               <=: istring "final")           <|> 
    PC.try (KW_Finally             <=: istring "finally")         <|> 
    PC.try (KW_Decimal             <=: istring "decimal")         <|> 
    PC.try (KW_For                 <=: istring "for")             <|> 
    PC.try (KW_If                  <=: istring "if")              <|> 
    PC.try (KW_Implements          <=: istring "implements")      <|> 
    PC.try (KW_Import              <=: istring "import")          <|> 
    PC.try (KW_Instanceof          <=: istring "instanceof")      <|> 
    PC.try (KW_Interface           <=: istring "interface")       <|> 
    PC.try (KW_Long                <=: istring "long")            <|> 
    PC.try (KW_New                 <=: istring "new")             <|> 
    PC.try (KW_Private             <=: istring "private")         <|> 
    PC.try (KW_Virtual             <=: istring "virtual")         <|> 
    PC.try (KW_Global              <=: istring "global")          <|> 
    PC.try (KW_Protected           <=: istring "protected")       <|> 
    PC.try (KW_Public              <=: istring "public")          <|> 
    PC.try (KW_Return              <=: istring "return")          <|> 
    PC.try (KW_Static              <=: istring "static")          <|> 
    PC.try (KW_Super               <=: istring "super")           <|> 
    PC.try (KW_Switch              <=: istring "switch on")       <|> 
    PC.try (KW_This                <=: istring "this")            <|> 
    PC.try (KW_Transient           <=: istring "transient")       <|> 
    PC.try (KW_Try                 <=: istring "try")             <|> 
    PC.try (KW_Void                <=: istring "void")            <|> 
    PC.try (KW_While               <=: istring "while")           <|> 
    PC.try (Op_Equals              <=: PS.string "==")            <|>
    PC.try (Op_LThanE              <=: PS.string "<=")            <|>
    PC.try (Op_GThanE              <=: PS.string ">=")            <|>
    PC.try (Op_BangE               <=: PS.string "!=")            <|>
    PC.try (Op_AAnd                <=: PS.string "&&")            <|>
    PC.try (Op_OOr                 <=: PS.string "||")            <|>
    PC.try (Op_PPlus               <=: PS.string "++")            <|>
    PC.try (Op_MMinus              <=: PS.string "--")            <|>
    PC.try (Op_LShift              <=: PS.string "<<")            <|>
    PC.try (Op_PlusE               <=: PS.string "+=")            <|>
    PC.try (Op_MinusE              <=: PS.string "-=")            <|>
    PC.try (Op_StarE               <=: PS.string "*=")            <|>
    PC.try (Op_SlashE              <=: PS.string "/=")            <|>
    PC.try (Op_AndE                <=: PS.string "&=")            <|>
    PC.try (Op_OrE                 <=: PS.string "|=")            <|>
    PC.try (Op_CaretE              <=: PS.string "^=")            <|>
    PC.try (Op_PercentE            <=: PS.string "%=")            <|>
    PC.try (Op_LShiftE             <=: PS.string "<<=")           <|>
    PC.try (Op_RShiftE             <=: PS.string ">>=")           <|>
    PC.try (Op_RRShiftE            <=: PS.string ">>>=")          <|>
    PC.try (OpenParen              <=: PS.char '(')               <|>
    PC.try (CloseParen             <=: PS.char ')')               <|>
    PC.try (OpenSquare             <=: PS.char '[')               <|>
    PC.try (CloseSquare            <=: PS.char ']')               <|>
    PC.try (OpenCurly              <=: PS.char '{')               <|>
    PC.try (CloseCurly             <=: PS.char '}')               <|>
    PC.try (Op_AtSign              <=: PS.char '@')               <|>
    PC.try (SemiColon              <=: PS.char ';')               <|>
    PC.try (Comma                  <=: PS.char ',')               <|>
    PC.try (Period                 <=: period    )                <|>
    PC.try (Op_Plus                <=: PS.char '+')               <|>
    PC.try (Op_Minus               <=: PS.char '-')               <|>
    PC.try (Op_Star                <=: PS.char '*')               <|>
    PC.try (Op_Slash               <=: PS.char '/')               <|>
    PC.try (Op_And                 <=: PS.char '&')               <|>
    PC.try (Op_Or                  <=: PS.char '|')               <|>
    PC.try (Op_Caret               <=: PS.char '^')               <|>
    PC.try (Op_Percent             <=: PS.char '%')               <|>
    PC.try (Op_Equal               <=: PS.char '=')               <|> 
    PC.try (Op_GThan               <=: PS.char '>')               <|>
    PC.try (Op_LThan               <=: PS.char '<')               <|>
    PC.try (Op_Bang                <=: PS.char '!')               <|>
    PC.try (Op_Tilde               <=: PS.char '~')               <|>
    PC.try (Op_Query               <=: PS.char '?')               <|>
    (Op_Colon                      <=: PS.char ':')    
    where 
        period = (PS.char '.' <* PC.notFollowedBy PT.digit)
        identTok = IdentTok <<=: identifier
            
javaLexer :: TokenParser
javaLexer = PT.makeTokenParser javaLanguage

javaLanguage :: LanguageDef
javaLanguage = do 
    let 
        (LanguageDef javaStyle) = PL.javaStyle 
        javaStyleDef = LanguageDef $ javaStyle 
                        { reservedNames = javaReservedNames
                        , reservedOpNames = javaReservedOpNames
                        } 
    javaStyleDef
    
javaReservedNames = 
    [ "override" ,"with" ,"without" ,"inherit" ,"object" ,"time" ,"id" ,"date" ,"datetime" ,"when" ,"abstract" ,"integer" 
    , "assert" ,"boolean" ,"break" ,"blob" ,"case" ,"catch" ,"class" ,"const" ,"continue" ,"when" ,"double" ,"do" ,"else" 
    , "enum" ,"extends" ,"final" ,"finally" ,"decimal" ,"for" ,"if" ,"implements" ,"import" ,"instanceof" ,"interface" ,"long" 
    , "new" ,"private" ,"protected" ,"public" ,"return" ,"static" ,"super" ,"switch" ,"this" ,"transient" ,"try" ,"void" ,"while" 
    , "virtual", "global", "string"
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
lParenTok      = OpenParen   <=: PS.char '('
rParenTok      = CloseParen  <=: PS.char ')'
lSquareTok     = OpenSquare  <=: PS.char '['
rSquareTok     = CloseSquare <=: PS.char ']'
lBraceTok      = OpenCurly   <=: PS.char '{'
rBraceTok      = CloseCurly  <=: PS.char '}'
semiColonTok   = SemiColon   <=: PS.char ';'
atTok          = Op_AtSign   <=: PS.char '@'
commTok        = Comma       <=: PS.char ','
periodTok      = Period      <=: (PS.char '.' <* PC.notFollowedBy PT.digit)

keywordTable = HS.fromArray javaReservedNames
operatorTable = HS.fromArray javaReservedOpNames
isKeyword = flip HS.member keywordTable
isOperator = flip HS.member operatorTable

-- identOrKeyword :: P (L Token)
-- identOrKeyword = do
--     p <- position
--     s <- identifier
--     pure $ 
--         if isKeyword s 
--         then (L (Pos p) $ KeywordTok s) 
--         else (L (Pos p) $ IdentTok s)

dot :: P String 
dot = javaLexer.dot 

identifier :: P String
identifier = javaLexer.identifier
    -- arrChar <- Array.cons <$> javaLetter <*> Array.many (PC.try PT.alphaNum <|> PS.oneOf ['_', '$'])
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

-- parses a semicolon
semi :: P String
semi = javaLexer.semi    

javaLetter :: P Char
javaLetter = PS.satisfy (\c -> isAlpha c || c == '$' || c == '_')

 -- parses whitespace
whiteSpace :: P Unit
whiteSpace = javaLexer.whiteSpace 

-- Apex does not support binary, octal, or hex based on the link below 
-- https://salesforce.stackexchange.com/questions/151169/how-do-i-assign-values-to-properties-using-hexadecimal-notation-using-apex
-- It seem like a Encoding Util class must be used for hex convertion only
-- https://developer.salesforce.com/docs/atlas.en-us.apexcode.meta/apexcode/apex_classes_restful_encodingUtil.htm
integerLiteral :: P Int 
-- integerLiteral = javaLexer.integer 
integerLiteral = do 
    x <- decimalIntegerLiteral  <* PC.notFollowedBy (PS.char '.')
    pure $ x

doubleLiteral :: P Number 
doubleLiteral = javaLexer.float
-- doubleLiteral = unsafeCoerce <$> PC.choice $ map PC.try 
--     [ (digitsStr <> dot <> digitsStr) ]

longLiteral :: P BigInt.BigInt 
longLiteral = do 
    i <- zero <|> digitsStr
    _ <- integerTypeSuffix
    maybe (fail "Could not read long integer") pure $ BigInt.fromString i 
    where 
        zero = PS.char '0' *> pure "0"

stringLiteral :: P String
stringLiteral = do 
    let stringCharacter = PS.satisfy (\c -> c /= '\'' && c /= '\\') 
    fromCharArray <$> (PS.char '\'' *> Array.many (stringCharacter <|> escapeChar) <* PS.char '\'')

decimalIntegerLiteral :: P Int 
decimalIntegerLiteral = do 
    i <- decimalNumeral
    _ <- PC.optional integerTypeSuffix
    pure i

boolLiteral :: P Boolean 
boolLiteral = readBool <$> (PC.choice <<< map PS.string) ["true", "false"]
    where 
        readBool "true"  = true
        readBool _       = false
    
opLiteral :: P String
-- opLiteral = (PC.choice <<< map PS.string) javaReservedOpNames
opLiteral = do
    let (LanguageDef javaLang) = javaLanguage
    ohead <- javaLang.opStart
    obody <- Array.many $ javaLang.opLetter
    let o = fromCharArray $ Array.cons ohead obody
    if isOperator o 
    then pure o
    else fail ("Unknown operator" <> o)

escapeChar :: P Char
escapeChar = PC.choice (map parseEsc escMap)
        where 

            parseEsc :: Tuple Char Char -> P Char
            parseEsc (Tuple c code) = PC.try (PS.char '\\' *>  PS.char c $> code)

            -- escape code tables
            escMap :: Array (Tuple Char Char)
            escMap = Array.zip [   'a',   'b',   'f',  'n',  'r',  't',   'v', '\\', '\"', '\'' ]
                            [ '\x7', '\x8', '\xC', '\n', '\r', '\t', '\xB', '\\', '\"', '\'' ]

decimalNumeral :: P Int 
decimalNumeral = zero <|> digits
    where 
        zero      = PS.char '0' *> pure 0

digits :: P Int 
digits = unsafeCoerce <$> digitsStr 
    

digitsStr :: P String 
digitsStr = many1 PT.digit >>= (pure <<< fromCharArray <<< toUnfoldable) 

integerTypeSuffix :: P Char 
integerTypeSuffix = PS.char 'l' <|> PS.char 'L'


