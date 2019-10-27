module Language.Types where 

import Prelude
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser (Parser)

data L a = L Pos a

newtype Pos = Pos Position 

derive instance newtypePos :: Newtype Pos _
derive instance genericL :: Generic (L a) _

derive instance eqPos :: Eq Pos 
derive instance eqL :: Eq a => Eq (L a)

instance showL :: Show a => Show (L a) where 
    show = genericShow

instance showPos :: Show Pos where 
    show (Pos (Position p)) = "(" <> show p.line <> "," <> show p.column <> ")"
