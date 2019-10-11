module Language.Apex.Lexer where 

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Array as Array
import Data.Char.Unicode (isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashSet as HS
import Data.Int as Int
import Data.List (List, toUnfoldable, someRec)
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (logShow, log)
import Text.Parsing.Parser (Parser, runParser, position, fail)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Language as PL
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser)
import Text.Parsing.Parser.Token as PT

type P = Parser String 

data L a = L Position a

data Token  = 
    -- Keywords
    KeywordTok String 
    -- KW_Abstract
    -- | KW_AnnInterface | KW_Assert | KW_Boolean | KW_Break | KW_Byte | KW_Case | KW_Catch | KW_Char | KW_Class | KW_Const 
    -- | KW_Continue | KW_Default | KW_Do | KW_Double | KW_Else | KW_Enum | KW_Extends | KW_Final | KW_Finally | KW_Float 
    -- | KW_For | KW_Goto | KW_If | KW_Implements | KW_Import | KW_Instanceof | KW_Int | KW_Interface | KW_Long | KW_Native 
    -- | KW_New | KW_Package | KW_Private | KW_Protected | KW_Public | KW_Return | KW_Short | KW_Static | KW_Strictfp | KW_Super 
    -- | KW_Switch | KW_Synchronized | KW_This | KW_Throw | KW_Throws | KW_Transient | KW_Try | KW_Void | KW_Volatile | KW_While

    -- -- Separators
    -- | OpenParen | CloseParen | OpenSquare | CloseSquare | OpenCurly | CloseCurly | SemiColon | Comma | Period | LambdaArrow | MethodRefSep

    -- Literals
    | IntTok Int | LongTok Int | DoubleTok Number | FloatTok Number | CharTok Char | StringTok String | BoolTok Boolean | NullTok

    -- -- Identifiers
    | IdentTok String

    -- -- Operators
    -- | Op_Equal | Op_GThan | Op_LThan | Op_Bang | Op_Tilde | Op_Query | Op_Colon | Op_Equals | Op_LThanE | Op_GThanE | Op_BangE | Op_AAnd 
    -- | Op_OOr | Op_PPlus | Op_MMinus | Op_Plus | Op_Minus | Op_Star | Op_Slash | Op_And | Op_Or | Op_Caret | Op_Percent | Op_LShift | Op_PlusE 
    -- | Op_MinusE | Op_StarE | Op_SlashE | Op_AndE | Op_OrE | Op_CaretE | Op_PercentE | Op_LShiftE | Op_RShiftE | Op_RRShiftE | Op_AtSign

derive instance genericToken :: Generic Token _
derive instance eqToken :: Eq Token 

derive instance genericL :: Generic (L a) _
derive instance eqL :: Eq a => Eq (L a)

instance showL :: Show a => Show (L a) where 
    show = genericShow

instance showToken :: Show Token where 
    show = genericShow

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
    , "finally" , "float" , "for" , "if" , "id" , "integer" , "implements" , "instanceof" , "interface" , "long" , "new" 
    , "object" , "private" , "protected" , "public" , "return" , "static" , "super" , "switch" , "string" , "time" , "this" 
    , "throw" , "throws" , "transient" , "try" , "void" , "while" 
    ]

javaReservedOpNames =
    [ "=" , ">" , "<" , "!" , "~" , "?" , ":" , "==" , "===" , "<=" , ">=" , "!=" , "!==" , "&&" , "||" , "++" , "--" , "+" 
    , "-" , "*" , "/" , "&" , "|" , "^" , "%" , "<<" , ">>" , ">>>" , "+=" , "-=" , "*=" , "/=" , "&=" , "|=" , "^=" , "%=" 
    , "<<=" , ">>=" , ">>>=" , "@" 
    ]

lexJava :: String -> Effect (Array (L Token))
lexJava = runTokenizer

nextToken :: P (L Token)
nextToken = PC.choice $ map (javaLexer.lexeme <<< PC.try) [identOrKeyword]

tokenize :: P (Array (L Token))
tokenize =  javaLexer.whiteSpace *> Array.many nextToken <* PS.eof

runTokenizer :: String -> Effect (Array (L Token))
runTokenizer s = case runParser s tokenize of
                   Left pe -> logShow pe *> pure []
                   Right toks -> log "Right" *> pure toks

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
        then (L p $ KeywordTok s) 
        else (L p $ IdentTok s)

-- | Match one or more times.
many1 :: forall a. P a -> P (List a)
many1 = someRec

-- parses an identifier
-- identifier :: P String  
-- identifier = javaLexer.identifier
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
parens :: forall t. Parser String t -> Parser String t
parens = javaLexer.parens      

-- parses an integer
integer :: P Int
integer = javaLexer.integer     

-- parses a semicolon
semi :: P String
semi = javaLexer.semi    

javaLetter :: P Char
javaLetter = PS.satisfy (\c -> isAlpha c || c == '$' || c == '_')

 -- parses whitespace
whiteSpace :: P Unit
whiteSpace = javaLexer.whiteSpace 

integerLiteral :: P Token 
integerLiteral = do 
    x <- decimalIntegerLiteral
    _ <- PS.whiteSpace
    pure  $ IntTok x

decimalIntegerLiteral :: P Int 
decimalIntegerLiteral = do 
    i <- decimalNumeral
    _ <- PC.optional integerTypeSuffix
    pure i

decimalNumeral :: P Int 
decimalNumeral = zero <|> manyDigit
    where 
        zero      = PS.char '0' *> pure 0
        manyDigit = many1 PT.digit >>= \d -> maybe (fail "Could not parse decimal") pure (readDigit d)

        readDigit :: List Char -> Maybe Int 
        readDigit = Int.fromString <<< fromCharArray <<< toUnfoldable 

integerTypeSuffix :: P Char 
integerTypeSuffix = PS.char 'l' <|> PS.char 'L'

infixr 6 mkToken as :=> 

infixr 6 mkTokenWith as :=>> 

mkToken :: forall a. Token -> P a -> P (L Token)
mkToken t p = do
    pos <- position
    _ <- p
    pure (L pos t)

mkTokenWith :: forall a. (a -> Token) -> P a -> P (L Token)
mkTokenWith t p = do
    pos <- position
    m <- p
    pure (L pos $ t m)
