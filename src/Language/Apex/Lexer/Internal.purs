module Language.Apex.Lexer.Internal where 

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Data.Char.Unicode (isAlpha)
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

identOrKeyword :: P (L Token)
identOrKeyword = do
    p <- position
    s <- identifier
    pure $ 
        if isKeyword s 
        then (L (Pos p) $ KeywordTok s) 
        else (L (Pos p) $ IdentTok s)

dot :: P String 
dot = javaLexer.dot 

identifier :: P String
identifier = do 
    arrChar <- Array.cons <$> javaLetter <*> Array.many (PT.alphaNum <|> PS.oneOf ['_', '$'])
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


