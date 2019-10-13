module Language.Apex.Syntax where 

import Prelude 
import Data.List.Types 
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.Apex.Syntax.Types 

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

----------------------- Variable Declation Types -----------------------------------

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl
    = VarDecl VarDeclId (Maybe VarInit)

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
    = VarId Ident
    | VarDeclArray VarDeclId
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.

-- | Explicit initializer for a variable declaration.
data VarInit
    = InitExp Exp
    | InitArray ArrayInit

data ArrayInit
    = ArrayInit (Array VarInit)

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier
    = Public
    | Private
    | Protected
    | Abstract
    | Final
    | Static
    | Transient

derive instance genericOp :: Generic Op _
derive instance genericExp :: Generic Exp _
derive instance genericVarDecl :: Generic VarDecl _
derive instance genericVarDeclId :: Generic VarDeclId _
derive instance genericVarInit :: Generic VarInit _
derive instance genericArrayInit :: Generic ArrayInit _
derive instance genericModifier :: Generic Modifier _

derive instance eqOp :: Eq Op
derive instance eqExp :: Eq Exp
derive instance eqVarDecl :: Eq VarDecl
derive instance eqVarDeclId :: Eq VarDeclId 
derive instance eqVarInit :: Eq VarInit
derive instance eqArrayInit :: Eq ArrayInit
derive instance eqModifier :: Eq Modifier

instance showOp :: Show Op where 
    show = genericShow
    
instance showVarDecl :: Show VarDecl where 
    show = genericShow 

instance showVarDeclId:: Show VarDeclId where 
    show  (VarId i) = "(VarId " <> show i <> ")"
    show (VarDeclArray v) = "(VarDeclArray " <> show v <> ")"

instance showVarInit :: Show VarInit where 
    show (InitExp e) = "(InitExp "  <> show e <> ")"
    show (InitArray a) = "(InitArray " <> show a <> "0"

instance showArrayInit :: Show ArrayInit where 
    show = genericShow 

instance showExp :: Show Exp where 
    show exp = case exp of 
        Lit lit -> show lit 
        (BinOp x op y) -> show x <> " " <> show op <> " " <> show y

instance showModifier :: Show Modifier where
   show Public = "public" 
   show Private = "private"
   show Protected = "protected"
   show Abstract = "abstract"
   show Final = "final"
   show Static = "static"
   show Transient = "transient"
