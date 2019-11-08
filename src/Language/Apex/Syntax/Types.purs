module Language.Apex.Syntax.Types where 

import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.List.Lazy (List)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.SOQL.Syntax (Query)


-- | A literal denotes a fixed, unchanging value.
data Literal
    = Integer Int
    | Double Number
    | Long BigInt
    | Boolean Boolean
    | String String
    | SOQL Query
    | Null

data DML = Update | Insert | Upsert | Delete | Undelete | Merge

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
    = PrimType PrimType
    | RefType RefType

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
    = ClassRefType ClassType
    {- | TypeVariable Ident -}
    | ArrayType Type

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
    = ClassType (List (Tuple Ident (List TypeArgument)))


-- | Type arguments may be either reference types or wildcards.
data TypeArgument = ActualType RefType

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam 
    = TypeParam Ident (List RefType)

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
    = BooleanT
    | ObjectT
    | DecimalT 
    | IntegerT
    | StringT
    | LongT
    | IdT
    | BlobT
    | DateT
    | DatetimeT
    | TimeT
    | DoubleT


derive instance genericLiteral :: Generic Literal _
derive instance genericPrimType :: Generic PrimType _
derive instance genericType :: Generic Type _
derive instance genericRefType :: Generic RefType _
derive instance genericClassType :: Generic ClassType _
derive instance genericTypeArgument :: Generic TypeArgument _
derive instance genericTypeParam :: Generic TypeParam _
derive instance genericDML :: Generic DML _

derive instance eqRefType :: Eq RefType
derive instance eqClassType :: Eq ClassType
derive instance eqTypeArgument :: Eq TypeArgument
derive instance eqTypeParam :: Eq TypeParam
derive instance eqType :: Eq Type
derive instance eqPrimType :: Eq PrimType
derive instance eqLiteral :: Eq Literal
derive instance eqDML :: Eq DML

instance showRefType :: Show RefType where 
    show (ClassRefType c) = "ClassRefType " <> show c 
    show x = genericShow x

instance showClassType :: Show ClassType where 
    show = genericShow 

instance showTypeParam :: Show TypeParam where 
    show = genericShow 

instance showTypeArgument :: Show TypeArgument where 
    show = genericShow 

instance showType :: Show Type where 
    show = genericShow 

instance showPrimType :: Show PrimType where 
    show BooleanT = "boolean"
    show ObjectT = "object"
    show DecimalT = "decimal"
    show IntegerT = "integer"
    show StringT = "string"
    show LongT = "long"
    show IdT = "id"
    show BlobT = "blob"
    show DateT = "date"
    show DatetimeT = "datetime"
    show TimeT = "time"
    show DoubleT = "double"

instance showLiteral :: Show Literal where 
    show = genericShow 

instance showDML :: Show DML where 
    show = genericShow 
-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident String

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name (List Ident)

derive instance genericIdent :: Generic Ident _
derive instance genericName:: Generic Name _

derive instance eqIdent :: Eq Ident
derive instance eqName :: Eq Name

instance showIdent :: Show Ident where 
    show = genericShow 

instance showName :: Show Name where 
    show = genericShow 