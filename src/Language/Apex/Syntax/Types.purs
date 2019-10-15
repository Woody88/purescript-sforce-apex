module Language.Apex.Syntax.Types where 

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | A literal denotes a fixed, unchanging value.
data Literal
    = Integer Int
    | Double Number
    | Long BigInt
    | Boolean Boolean
    | String String
    | Null

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
    = BooleanT
    | ObjectT
    | DecimalT 
    | IntegerT
    | LongT
    | BlobT
    | DateT
    | DatetimeT
    | TimeT
    | DoubleT


derive instance genericLiteral :: Generic Literal _
derive instance genericPrimType :: Generic PrimType _
derive instance genericType :: Generic Type _

derive instance eqType :: Eq Type
derive instance eqPrimType :: Eq PrimType
derive instance eqLiteral :: Eq Literal

instance showType :: Show Type where 
    show = genericShow 

instance showPrimType :: Show PrimType where 
    show = genericShow 

instance showLiteral :: Show Literal where 
    show = genericShow 
-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident String

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name (Array Ident)

derive instance genericIdent :: Generic Ident _
derive instance genericName:: Generic Name _

derive instance eqIdent :: Eq Ident
derive instance eqName :: Eq Name

instance showIdent :: Show Ident where 
    show = genericShow 

instance showName :: Show Name where 
    show = genericShow 