module Language.Apex.Lexer where 

import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Maybe (Maybe, maybe)
import Data.Int as Int
import Data.List (List, toUnfoldable, someRec)
import Data.String.CodeUnits (fromCharArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Language as PL
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Token as PT 
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser)
import Text.Parsing.Parser.String as PS

type P = Parser String 

data Token  = 
    -- Keywords
    KW_Abstract
    | KW_AnnInterface | KW_Assert | KW_Boolean | KW_Break | KW_Byte | KW_Case | KW_Catch | KW_Char | KW_Class | KW_Const 
    -- | KW_Continue | KW_Default | KW_Do | KW_Double | KW_Else | KW_Enum | KW_Extends | KW_Final | KW_Finally | KW_Float 
    -- | KW_For | KW_Goto | KW_If | KW_Implements | KW_Import | KW_Instanceof | KW_Int | KW_Interface | KW_Long | KW_Native 
    -- | KW_New | KW_Package | KW_Private | KW_Protected | KW_Public | KW_Return | KW_Short | KW_Static | KW_Strictfp | KW_Super 
    -- | KW_Switch | KW_Synchronized | KW_This | KW_Throw | KW_Throws | KW_Transient | KW_Try | KW_Void | KW_Volatile | KW_While

    -- -- Separators
    -- | OpenParen | CloseParen | OpenSquare | CloseSquare | OpenCurly | CloseCurly | SemiColon | Comma | Period | LambdaArrow | MethodRefSep

    -- Literals
    | IntTok Int | LongTok Int | DoubleTok Number | FloatTok Number | CharTok Char | StringTok String | BoolTok Boolean | NullTok

    -- -- Identifiers
    -- | IdentTok String

    -- -- Operators
    -- | Op_Equal | Op_GThan | Op_LThan | Op_Bang | Op_Tilde | Op_Query | Op_Colon | Op_Equals | Op_LThanE | Op_GThanE | Op_BangE | Op_AAnd 
    -- | Op_OOr | Op_PPlus | Op_MMinus | Op_Plus | Op_Minus | Op_Star | Op_Slash | Op_And | Op_Or | Op_Caret | Op_Percent | Op_LShift | Op_PlusE 
    -- | Op_MinusE | Op_StarE | Op_SlashE | Op_AndE | Op_OrE | Op_CaretE | Op_PercentE | Op_LShiftE | Op_RShiftE | Op_RRShiftE | Op_AtSign

derive instance genericToken :: Generic Token _
derive instance eqToken :: Eq Token 

instance showToken :: Show Token where 
    show = genericShow

-- | Match one or more times.
-- many1 :: forall a. Parser a -> Parser (List a)
many1 = someRec


lexer :: TokenParser
lexer = PT.makeTokenParser languageDef

-- parses an identifier
identifier :: P String  
identifier = lexer.identifier

-- parses a reserved name
reserved :: String -> P Unit
reserved = lexer.reserved    

 -- parses an operator
reservedOp :: String -> P Unit
reservedOp = lexer.reservedOp 

-- parses surrounding parenthesis:
parens :: forall t. Parser String t -> Parser String t
parens = lexer.parens      

-- parses an integer
integer :: P Int
integer = lexer.integer     

-- parses a semicolon
semi :: P String
semi = lexer.semi        

 -- parses whitespace
whiteSpace :: P Unit
whiteSpace = lexer.whiteSpace 

languageDef :: LanguageDef
languageDef = do 
    let 
        (LanguageDef javaStyle) = PL.javaStyle 
        javaStyleDef = LanguageDef $ javaStyle 
                        { reservedNames = reservedOpNames'
                        , reservedOpNames = reservedOpNames'
                        } 
    javaStyleDef
    where 
        reservedNames' = 
            [ "abstract" , "assert" , "boolean" , "break" , "blob" , "byte" , "case" , "catch" , "char" , "class" , "const" 
            , "continue" , "date" , "dfatetime" , "default" , "decimal" , "do" , "double" , "else" , "enum" , "extends" , "final" 
            , "finally" , "float" , "for" , "if" , "id" , "integer" , "implements" , "instanceof" , "interface" , "long" , "new" 
            , "object" , "private" , "protected" , "public" , "return" , "static" , "super" , "switch" , "string" , "time" , "this" 
            , "throw" , "throws" , "transient" , "try" , "void" , "while" 
            ]
        
        reservedOpNames' =
            [ "=" , ">" , "<" , "!" , "~" , "?" , ":" , "==" , "===" , "<=" , ">=" , "!=" , "!==" , "&&" , "||" , "++" , "--" , "+" 
            , "-" , "*" , "/" , "&" , "|" , "^" , "%" , "<<" , ">>" , ">>>" , "+=" , "-=" , "*=" , "/=" , "&=" , "|=" , "^=" , "%=" 
            , "<<=" , ">>=" , ">>>=" , "@" 
            ]

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