module Language.Apex.Lexer.Types where 

import Data.BigInt
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Pos (Position(..))

type P = Parser String 

data L a = L Pos a

newtype Pos = Pos Position 

data Token  = 
    -- Keywords
    KW_Abstract
    | KW_Assert | KW_Boolean | KW_Break | KW_Blob | KW_Case | KW_Catch | KW_Char | KW_Class | KW_Const | KW_When
    | KW_Continue | KW_Default | KW_Do | KW_Double | KW_Else | KW_Enum | KW_Extends | KW_Final | KW_Finally | KW_Decimal 
    | KW_For | KW_If | KW_Implements | KW_Import | KW_Instanceof | KW_Integer | KW_Interface | KW_Long 
    | KW_New | KW_Private | KW_Protected | KW_Public | KW_Return | KW_Short | KW_Static | KW_Super 
    | KW_Switch | KW_This | KW_Throw | KW_Throws | KW_Transient | KW_Try | KW_Void | KW_While 
    | KW_Object | KW_Time | KW_ID | KW_Date | KW_Datetime

    -- Annotations
    | KW_AnnAuraEnabled | KW_AnnDeprecated | KW_AnnFuture | KW_AnnInvocableMethod | KW_AnnInvocableVar | KW_AnnIsTest 
    | KW_AnnNamespaceAccess | KW_AnnReadOnly | KW_AnnRemoteAction | KW_AnnSuppressWarnings | KW_AnnTestSetup | KW_AnnTestVisible

    -- Separators
    | OpenParen | CloseParen | OpenSquare | CloseSquare | OpenCurly | CloseCurly | SemiColon | Comma | Period 

    -- Literals 
    | IntTok Int | LongTok BigInt | DoubleTok Number | CharTok Char | StringTok String | BoolTok Boolean | NullTok

    -- Identifiers
    | IdentTok String
 
    -- Operators
    | OpTok String
    | Op_Equal | Op_GThan | Op_LThan | Op_Bang | Op_Tilde | Op_Query | Op_Colon | Op_Equals | Op_LThanE | Op_GThanE | Op_BangE | Op_AAnd 
    | Op_OOr | Op_PPlus | Op_MMinus | Op_Plus | Op_Minus | Op_Star | Op_Slash | Op_And | Op_Or | Op_Caret | Op_Percent | Op_LShift | Op_PlusE 
    | Op_MinusE | Op_StarE | Op_SlashE | Op_AndE | Op_OrE | Op_CaretE | Op_PercentE | Op_LShiftE | Op_RShiftE | Op_RRShiftE | Op_AtSign

derive instance newtypePos :: Newtype Pos _
derive instance genericToken :: Generic Token _
derive instance eqToken :: Eq Token 
derive instance eqPos :: Eq Pos 

derive instance genericL :: Generic (L a) _
derive instance eqL :: Eq a => Eq (L a)

instance showPos :: Show Pos where 
    show (Pos (Position p)) = "(" <> show p.line <> "," <> show p.column <> ")"

instance showL :: Show a => Show (L a) where 
    show = genericShow

instance showToken :: Show Token where 
    show = genericShow

