module Language.Apex.Lexer.Internal where

import Language.Apex.AST
import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Language
import Text.Parsing.Parser.Token

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Char.Unicode (isAlpha, toLower, toUpper)
import Data.HashSet as S
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser.Combinators ((<?>), choice, notFollowedBy, try)
import Text.Parsing.Parser.String (char, oneOf, satisfy, string)

type P = Parser String 

infixr 6 mkToken as <=:
infixr 6 mkTokenWith as <<=:
    
apexReservedNames = 
    [ "abstract", "assert", "boolean", "break", "case", "catch", "when", "class"
    , "continue", "default", "do", "double", "else", "enum", "virtual", "override"
    , "extends", "final", "finally", "long", "for", "if", "implements", "global"
    , "instanceof", "integer", "interface", "long", "new", "private"
    , "protected", "public", "return", "static", "super", "with sharing", "without sharing", "inherited sharing"
    , "switch", "this", "throw","transient", "try", "void", "decimal", "while"
    ]

apexeservedOpNames = 
    [ "!=", "!", ">>>=", ">>>", ">>=", ">=", ">>", ">","=="
    , "=", "|", "|=", "||", "&&", "&=", "&", "^=", "^","%"
    , "%=", "*=", "*", "++", "+=", "+", "--", "-=", "-","/="
    , "/", ":", "<<=", "<<", "<=", "<","?"
    ]

keywordTable = S.fromFoldable apexReservedNames
operatorTable = S.fromFoldable apexeservedOpNames
isKeyword = flip S.member keywordTable
isOperator = flip S.member operatorTable

javaLanguage :: LanguageDef 
javaLanguage = do 
    let javaStyl = unGenLanguageDef javaStyle
    LanguageDef $ 
        javaStyl { reservedNames     = apexReservedNames
                 , reservedOpNames   = apexeservedOpNames
                 , opStart           = oneOf $ toCharArray "!%&*+/<=>?^|-:."
                 , opLetter          = oneOf $ toCharArray "&*+/=^|-"
                 }

javaLexer :: TokenParser
javaLexer = makeTokenParser javaLanguage

identOrKeyword :: P Token
identOrKeyword = do
    p <- position
    s <- try sharingModeLiteral <|> identifier
    pure $ if isKeyword s
        then (Keyword s /\ p)
        else (TokIdent s /\ p)

sharingModeLiteral :: P String 
sharingModeLiteral = withSharing <|> withoutSharing <|> inherithSharing
    where  
        withSharing     = istring "with sharing"
        withoutSharing  = istring "without sharing"
        inherithSharing = istring "inherit sharing"

tokInt      =  TokInteger       <<=: integerLiteral
tokDouble   =  TokDouble    <<=: doubleLiteral
tokString   =  TokString    <<=: stringLiteral
tokLong     =  TokLong      <<=: longLiteral
tokBool     =  TokBool      <<=: boolLiteral
op          =  Operator     <<=: opLiteral
tokNull     =  TokNull      <=: string "null"
lParen      =  LParen       <=: char '('
rParen      =  RParen       <=: char ')'
lSquare     =  LSquare      <=: char '['
rSquare     =  RSquare      <=: char ']'
lBrace      =  LBrace       <=: char '{'
rBrace      =  RBrace       <=: char '}'
semiColon   =  SemiColon    <=: char ';'
at          =  At           <=: char '@'
comm        =  Comma        <=: char ','
period      =  Period       <=: (char '.' <* notFollowedBy digit)

-- javaSingleChar :: Parser Char
-- javaSingleChar = noneOf "\\'" <|> unicodeEscape

-- unicodeEscape :: Parser Char
-- unicodeEscape = do
--     _ <- string "\\u"
--     cs <- count 4 anyChar
--     if all isHexDigit cs then
--         return ((fst . head) $ readLitChar ("\\x" ++ cs))
--     else
--         parserFail "Bad unicode escape"

escapeChar :: P Char
escapeChar = choice (map parseEsc escMap)
    where 
        parseEsc :: Tuple Char Char -> P Char
        parseEsc (Tuple c code) = try (char '\\' *>  char c $> code)

        -- escape code tables
        escMap :: Array (Tuple Char Char)
        escMap = Array.zip [   'a',   'b',   'f',  'n',  'r',  't',   'v', '\\', '\"', '\'' ]
                            [ '\x7', '\x8', '\xC', '\n', '\r', '\t', '\xB', '\\', '\"', '\'' ]


-- octalEscape :: Parser Char
-- octalEscape =
--     char '\\' *> (choice . map try)
--        [readOctal  <$> octalDigit <* eof,
--         readOctal2 <$> octalDigit  <*> octalDigit <* eof,
--         readOctal3 <$> zeroToThree <*> octalDigit <*> octalDigit]
--         where
--             zeroToThree       = drange '0' '3'
--             octalDigit        = drange '0' '7'
--             readOctal  s      = chr (read ("0o" ++ [s]) :: Int)
--             readOctal2 s t    = chr (read ("0o" ++ [s, t]) :: Int)
--             readOctal3 s t u  = chr (read ("0o" ++ [s, t, u]) ::Int)

-- drange :: Char -> Char -> Parser Char
-- drange b e = satisfy (\c -> ord b <= ord c && ord c <= ord e)

stringLiteral :: P String
stringLiteral = do 
    let stringCharacter = satisfy (\c -> c /= '\'' && c /= '\\') 
    fromCharArray <$> (char '\'' *> Array.many (stringCharacter <|> escapeChar) <* char '\'')

-- charLiteral :: Parser Char
-- charLiteral = char '\'' *> (try escapeSequence <|> javaSingleChar) <* char '\''

javaLetter :: P Char
javaLetter = satisfy (\c -> isAlpha c || c == '$' || c == '_')

-- identifier :: P String
-- identifier = javaLexer.identifier -- (:) <$> javaLetter <*> many (alphaNum <|> oneOf "_$")

identifier :: P String
identifier = do 
    arrChar <- Array.cons <$> javaLetter <*> Array.many (try alphaNum <|> oneOf ['_', '$'])
    pure $ fromCharArray arrChar

integerLiteral :: P Int
integerLiteral = javaLexer.integer
-- integerLiteral = (read <$> (choice . map try)
--     [ hexNumeral, binaryNumeral, octalNumeral, decimalNumeral ])
--     <* notFollowedBy (char '.')

-- decimalNumeral =  string "0" <* notFollowedBy digit
--               <|> ((:) <$> drange '1' '9' <* underscore <*>
--                     many (digit <* underscore))

-- octalNumeral = ("0o" ++) <$> (string "0" *> many1 octDigit)

-- binaryNumeral = bin2dec <$> (string "0b" *> many1 (oneOf "01" <* underscore))

-- bin2dec = show . foldr (\c s -> s * 2 + c::Integer) 0 . reverse . map c2i
--             where c2i c = if c == '0' then 0 else 1

-- hexNumeral = (++) <$> string "0x" <*> many1 (hexDigit <* underscore)
-- underscore = optional (many (char '_'))

doubleLiteral :: P Number
doubleLiteral = javaLexer.float 

longLiteral :: P BigInt 
longLiteral = do 
    i <- zero <|> digitsStr
    _ <- integerTypeSuffix
    maybe (fail "Could not read long integer") pure $ BigInt.fromString i 
    where 
        zero = char '0' *> pure "0"

integerTypeSuffix :: P Char 
integerTypeSuffix = char 'l' <|> char 'L'

-- floatLiteral = decimalFloatLiteral <|> hexFloatLiteral where
--     decimalFloatLiteral = choice $ map try
--      [
--         a5 <$> digits <*> dot <*> can digits <*> can ex <*> can suffix
--       , a4 <$> dot <*> digits <*> can ex <*> can suffix
--       , a3 <$> digits <*> ex <*> can suffix
--       , a3 <$> digits <*> can ex <*> suffix
--      ]

--     hexFloatLiteral = (++) <$> hexSignificand <*> binex
--     hexSignificand = choice $ map try
--      [
--         (++) <$> hexNumeral <*> can dot
--       , a6 <$> char '0' <*> oneOf "xX" <*> can hexDigits <*> dot <*> hexDigits
--      ]
--     binex = a3 <$> pp <*> can sign <*> many1 digit
--     hexDigits = many1 hexDigit
--     digits = many1 digit
--     dot = string "."
--     ex = (\a b c -> a : (b ++ c)) <$> oneOf "eE" <*> can sign <*> many1 digit
--     suffix = (:[]) <$> oneOf "fFdD"
--     sign = (:[]) <$> oneOf "+-"
--     pp = (:[]) <$> oneOf "pP"
--     can p = try p <|> return ""
--     a4 a b c d = a ++ b ++ c ++ d
--     a3 a b c = a ++ b ++ c
--     a5 a b c d e = a ++ b ++ c ++ d ++ e
--     a6 a b c d e = a:b:(c ++ d ++ e)

boolLiteral :: P Boolean 
boolLiteral = readBool <$> (choice <<< map string) ["true", "false"]
    where 
        readBool "true"  = true
        readBool _       = false

opLiteral :: P String
opLiteral = javaLexer.operator 

digitsStr :: P String 
digitsStr = Array.some digit >>= (pure <<< fromCharArray) 

mkTokenWith :: forall a. (a -> T) -> P a -> P Token
mkTokenWith t p = do
    pos <- position
    m   <- p
    pure (t m /\ pos)

mkToken :: forall a. T -> P a -> P Token
mkToken t p = do
    pos <- position 
    _   <- p
    pure (t /\ pos)

-- case insensitive match string 
istring :: String -> P String 
istring = caseInsensitiveString

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString s = try (fromCharArray <$> traverse caseInsensitiveChar charList ) <?> ("\"" <> s <> "\"")
    where 
        charList = toCharArray s