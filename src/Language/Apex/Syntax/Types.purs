module Language.Apex.Syntax.Types where 

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | A literal denotes a fixed, unchanging value.
data Literal
    = Int Int
    | Double Number
    | Long BigInt
    | Boolean Boolean
    | String String
    | Null

derive instance genericLiteral :: Generic Literal _

derive instance eqLiteral :: Eq Literal

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