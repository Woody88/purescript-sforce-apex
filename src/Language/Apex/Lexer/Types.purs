module Language.Apex.Lexer.Types where 

import Prelude 
import Data.BigInt
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Pos (Position)

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
    | IntTok Int | LongTok BigInt | DoubleTok Number | CharTok Char | StringTok String | BoolTok Boolean | NullTok

    -- -- Identifiers
    | IdentTok String
 
    -- -- Operators
    | OpTok String
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
