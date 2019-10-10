module Language.Apex.Syntax where 

import Prelude 
import Data.List.Types 
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Literal 
    = Int Int 
    | Word Int 
    | Float Number 
    | Double Number 
    | Boolean Boolean 
    | Char Char 
    | String String 
    | Null 
    
-- | A binary infix operator.
data Op = Mult  | Div   | Rem    | Add    | Sub   | LShift | RShift | RRShift
        | LThan | GThan | LThanE | GThanE | Equal | NotEq
        | And   | Or    | Xor    | CAnd   | COr

-- | An assignment operator.
data AssignOp = EqualA  | MultA   | DivA     | RemA | AddA | SubA
              | LShiftA | RShiftA | RRShiftA | AndA | XorA | OrA

data Exp 
    = Lit Literal
     -- | The application of a binary operator to two operand expressions.
    | BinOp Exp Op Exp

derive instance genericOp :: Generic Op _
derive instance genericLiteral :: Generic Literal _
derive instance genericExp :: Generic Exp _

derive instance eqLiteral :: Eq Literal
derive instance eqOp :: Eq Op
derive instance eqExp:: Eq Exp

instance showLiteral :: Show Literal where 
    show = genericShow 

instance showOp :: Show Op where 
    show = genericShow 

instance showExp :: Show Exp where 
    show exp = case exp of 
        Lit lit -> show lit 
        (BinOp x op y) -> show x <> " " <> show op <> " " <> show y