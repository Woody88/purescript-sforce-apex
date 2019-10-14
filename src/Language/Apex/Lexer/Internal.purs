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
import Unsafe.Coerce (unsafeCoerce)


class ReadToken a where 
    readToken :: P (L a)

instance readTokenT :: ReadToken Token where 
    readToken = 
        PC.try (IntTok        <<=: integerLiteral) <|>
        PC.try (DoubleTok     <<=: doubleLiteral)  <|>
        PC.try (LongTok       <<=: longLiteral)    <|>
        PC.try (StringTok     <<=: stringLiteral)  <|>
        PC.try (BoolTok       <<=: boolLiteral)    <|>
        PC.try (Period        <=: period     )     <|>
        PC.try (KW_AnnAuraEnabled         <=: str "@AuraEnabled" ) <|>
        PC.try (KW_AnnDeprecated          <=: str "@Deprecated" ) <|>
        PC.try (KW_AnnFuture              <=: str "@Future" ) <|>
        PC.try (KW_AnnInvocableMethod     <=: str "@InvocableMethod" ) <|>
        PC.try (KW_AnnInvocableVar        <=: str "@InvocableVariable" ) <|>
        PC.try (KW_AnnIsTest              <=: str "@IsTest" ) <|>
        PC.try (KW_AnnNamespaceAccess     <=: str "@NamespaceAccessible" ) <|>
        PC.try (KW_AnnReadOnly            <=: str "@ReadOnly" ) <|>
        PC.try (KW_AnnRemoteAction        <=: str "@RemoteAction" ) <|>
        PC.try (KW_AnnSuppressWarnings    <=: str "@SuppressWarnings" ) <|>
        PC.try (KW_AnnTestSetup           <=: str "@TestSetup" ) <|>
        PC.try (KW_AnnTestVisible         <=: str "@TestVisible" ) <|>
        PC.try (KW_Object     <=: str "object" ) <|>
        PC.try (KW_Time       <=: str "time") <|> 
        PC.try (KW_ID         <=: str "id") <|>
        PC.try (KW_Date       <=: str "date") <|> 
        PC.try (KW_Datetime   <=: str "datetime") <|>
        PC.try (KW_When       <=: str "when") <|> 
        PC.try (KW_Abstract   <=: str "abstract") <|> 
        PC.try (KW_Integer    <=: str "integer") <|> 
        PC.try (KW_Assert     <=: str "assert") <|> 
        PC.try (KW_Boolean    <=: str "boolean") <|> 
        PC.try (KW_Break      <=: str "break") <|> 
        PC.try (KW_Blob       <=: str "blob") <|> 
        PC.try (KW_Case       <=: str "case") <|> 
        PC.try (KW_Catch      <=: str "catch") <|> 
        PC.try (KW_Class      <=: str "class") <|> 
        PC.try (KW_Const      <=: str "const") <|> 
        PC.try (KW_Continue   <=: str "continue") <|> 
        PC.try (KW_Default    <=: str "default") <|> 
        PC.try (KW_Do         <=: str "do") <|> 
        PC.try (KW_Double     <=: str "double") <|> 
        PC.try (KW_Else       <=: str "else") <|> 
        PC.try (KW_Enum       <=: str "enum") <|> 
        PC.try (KW_Extends    <=: str "extends") <|> 
        PC.try (KW_Final      <=: str "final") <|> 
        PC.try (KW_Finally    <=: str "finally") <|> 
        PC.try (KW_Decimal    <=: str "decimal") <|> 
        PC.try (KW_For        <=: str "for") <|> 
        PC.try (KW_If         <=: str "if") <|> 
        PC.try (KW_Implements <=: str "implements") <|> 
        PC.try (KW_Import     <=: str "import") <|> 
        PC.try (KW_Instanceof <=: str "instanceof") <|> 
        PC.try (KW_Interface  <=: str "interface") <|> 
        PC.try (KW_Long       <=: str "long") <|> 
        PC.try (KW_New        <=: str "new") <|> 
        PC.try (KW_Private    <=: str "private") <|> 
        PC.try (KW_Protected  <=: str "protected") <|> 
        PC.try (KW_Public     <=: str "public") <|> 
        PC.try (KW_Return     <=: str "return") <|> 
        PC.try (KW_Static     <=: str "static") <|> 
        PC.try (KW_Super      <=: str "super") <|> 
        PC.try (KW_Switch     <=: str "switch") <|> 
        PC.try (KW_This       <=: str "this") <|> 
        PC.try (KW_Transient  <=: str "transient") <|> 
        PC.try (KW_Try        <=: str "try") <|> 
        PC.try (KW_Void       <=: str "void") <|> 
        PC.try (KW_While      <=: str "while") <|> 
        PC.try (OpenParen     <=: PS.char '(')     <|>
        PC.try (CloseParen    <=: PS.char ')')     <|>
        PC.try (OpenSquare    <=: PS.char '[')     <|>
        PC.try (CloseSquare   <=: PS.char ']')     <|>
        PC.try (OpenCurly     <=: PS.char '{')     <|>
        PC.try (CloseCurly    <=: PS.char '}')     <|>
        PC.try (Op_AtSign     <=: PS.char '@')     <|>
        PC.try (SemiColon     <=: PS.char ';')     <|>
        PC.try (Comma         <=: PS.char ',')     <|>
        PC.try (Period        <=: period    )     <|>
        PC.try (Op_Plus       <=: PS.char '+')     <|>
        PC.try (Op_Minus      <=: PS.char '-')     <|>
        PC.try (Op_Star       <=: PS.char '*')     <|>
        PC.try (Op_Slash      <=: PS.char '/')     <|>
        PC.try (Op_And        <=: PS.char '&')     <|>
        PC.try (Op_Or         <=: PS.char '|')     <|>
        PC.try (Op_Caret      <=: PS.char '^')     <|>
        PC.try (Op_Percent    <=: PS.char '%')     <|>
        PC.try (Op_Equal      <=: PS.char '=')     <|> 
        PC.try (Op_GThan      <=: PS.char '>')     <|>
        PC.try (Op_LThan      <=: PS.char '<')     <|>
        PC.try (Op_Bang       <=: PS.char '!')     <|>
        PC.try (Op_Tilde      <=: PS.char '~')     <|>
        PC.try (Op_Query      <=: PS.char '?')     <|>
        PC.try (Op_Colon      <=: PS.char ':')     <|>
        PC.try (Op_Equals     <=: PS.string "==")    <|>
        PC.try (Op_LThanE     <=: PS.string "<=")    <|>
        PC.try (Op_GThanE     <=: PS.string ">=")    <|>
        PC.try (Op_BangE      <=: PS.string "!=")    <|>
        PC.try (Op_AAnd       <=: PS.string "&&")    <|>
        PC.try (Op_OOr        <=: PS.string "||")    <|>
        PC.try (Op_PPlus      <=: PS.string "++")    <|>
        PC.try (Op_MMinus     <=: PS.string "--")    <|>
        PC.try (Op_LShift     <=: PS.string "<<")    <|>
        PC.try (Op_PlusE      <=: PS.string "+=")    <|>
        PC.try (Op_MinusE     <=: PS.string "-=")    <|>
        PC.try (Op_StarE      <=: PS.string "*=")    <|>
        PC.try (Op_SlashE     <=: PS.string "/=")    <|>
        PC.try (Op_AndE       <=: PS.string "&=")    <|>
        PC.try (Op_OrE        <=: PS.string "|=")    <|>
        PC.try (Op_CaretE     <=: PS.string "^=")    <|>
        PC.try (Op_PercentE   <=: PS.string "%=")    <|>
        PC.try (Op_LShiftE    <=: PS.string "<<=")   <|>
        PC.try (Op_RShiftE    <=: PS.string ">>=")   <|>
        PC.try (Op_RRShiftE   <=: PS.string ">>>=")  <|>
        identTok
        where 
            period = (PS.char '.' <* PC.notFollowedBy PT.digit)
            identTok = IdentTok <<=: identifier
            str  = caseInsensitiveString
            
-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar c = PS.char (toLower c) <|> PS.char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString s = PC.try (fromCharArray <$> traverse caseInsensitiveChar charList ) <?> ("\"" <> s <> "\"")
    where 
        charList = toCharArray s

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
    [ "abstract" , "assert" , "boolean" , "break" , "blob" , "byte" , "case" , "catch" , "char" , "class" , "const" 
    , "continue" , "date" , "dfatetime" , "default" , "decimal" , "do" , "double" , "else" , "enum" , "extends" , "final" 
    , "finally" , "for" , "if" , "id" , "integer" , "implements" , "instanceof" , "interface" , "long" , "new" 
    , "object" , "private" , "protected" , "public" , "return" , "static" , "super" , "switch" , "string" , "time" , "this" 
    , "throw" , "throws" , "transient" , "try" , "void" , "while", "when", "null"
    ]

javaReservedOpNames =
    [ "=" , ">" , "<" , "!" , "~" , "?" , ":" , "==" , "===" , "<=" , ">=" , "!=" , "!==" , "&&" , "||" , "++" , "--" , "+" 
    , "-" , "*" , "/" , "&" , "|" , "^" , "%" , "<<" , ">>" , ">>>" , "+=" , "-=" , "*=" , "/=" , "&=" , "|=" , "^=" , "%=" 
    , "<<=" , ">>=" , ">>>=" , "@" 
    ]


intTok         = IntTok      <<=: integerLiteral
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
identifier = do 
    arrChar <- Array.cons <$> javaLetter <*> Array.many (PC.try PT.alphaNum <|> PS.oneOf ['_', '$'])
    pure $ fromCharArray arrChar

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
integerLiteral = javaLexer.integer 
-- integerLiteral = do 
--     x <- decimalIntegerLiteral  <* PC.notFollowedBy (PS.char '.')
--     pure $ x

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


