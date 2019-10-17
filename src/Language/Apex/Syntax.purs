module Language.Apex.Syntax where 

import Prelude 
import Data.Tuple (Tuple(..))
import Data.List.Types 
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.Apex.Syntax.Types 

type Mod a = List Modifier -> a 

-- | Arguments to methods and constructors are expressions.
type Argument = Exp

data Tuple3 a b c = Tuple3 a b c 

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

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block = Block (List BlockStmt)

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt
    = LocalVars (List Modifier) Type (List VarDecl)
    -- BlockStmt Stmt
    -- | LocalClass ClassDecl

derive instance genericBlock :: Generic Block _ 
derive instance genericBlockStmt :: Generic BlockStmt _ 

derive instance eqBlockStmt :: Eq BlockStmt
derive instance eqBlock :: Eq Block

instance showBlockStmt :: Show BlockStmt where
    show = genericShow

instance showBlock :: Show Block where
    show = genericShow

----------------------- Declation Types -----------------------------------
-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl (List Modifier) Type (List VarDecl)
    -- -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl (List Modifier) (List TypeParam) (Maybe Type) Ident (List FormalParam) (Maybe Exp) MethodBody
    -- -- | A constructor is used in the creation of an object that is an instance of a class.
    -- | ConstructorDecl [Modifier] [TypeParam]              Ident [FormalParam] [ExceptionType] ConstructorBody
    -- -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    -- | MemberClassDecl ClassDecl
    -- -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    -- | MemberInterfaceDecl InterfaceDecl
-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody = MethodBody (Maybe Block)

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
    = ArrayInit (List VarInit)

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam = FormalParam (List Modifier) Type VarDeclId


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
    | Annotation Annotation

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue = EVVal VarInit

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation 
    = NormalAnnotation  { annName :: Name,  annKV :: List (Tuple Ident ElementValue) }
    | MarkerAnnotation  { annName :: Name }

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv
    = ThisInvoke             (List RefType) Argument
    | SuperInvoke            (List RefType) Argument
    | PrimarySuperInvoke Exp (List RefType) Argument

derive instance genericOp :: Generic Op _
derive instance genericExp :: Generic Exp _
derive instance genericVarDecl :: Generic VarDecl _
derive instance genericVarDeclId :: Generic VarDeclId _
derive instance genericVarInit :: Generic VarInit _
derive instance genericArrayInit :: Generic ArrayInit _
derive instance genericModifier :: Generic Modifier _
derive instance genericAnnotation :: Generic Annotation _
derive instance genericElementValue :: Generic ElementValue _
derive instance genericMethodBody :: Generic MethodBody _
derive instance genericMemberDecl :: Generic MemberDecl _
derive instance genericFormalParam :: Generic FormalParam _
derive instance genericExplConstrInv :: Generic ExplConstrInv _

derive instance eqExplConstrInv :: Eq ExplConstrInv
derive instance eqOp :: Eq Op
derive instance eqExp :: Eq Exp
derive instance eqVarDecl :: Eq VarDecl
derive instance eqVarDeclId :: Eq VarDeclId 
derive instance eqVarInit :: Eq VarInit
derive instance eqArrayInit :: Eq ArrayInit
derive instance eqModifier :: Eq Modifier
derive instance eqAnnotation :: Eq Annotation
derive instance eqElementValue :: Eq ElementValue
derive instance eqMethodBody :: Eq MethodBody
derive instance eqMemberDecl :: Eq MemberDecl
derive instance eqFormalParam :: Eq FormalParam

instance showExplConstrInv :: Show ExplConstrInv where 
    show = genericShow

instance showFormalParam :: Show FormalParam where 
    show = genericShow

instance showMemberDecl :: Show MemberDecl where 
    show = genericShow

instance showMethodBody :: Show MethodBody where 
    show = genericShow

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

instance showElementValue :: Show ElementValue where 
    show = genericShow 

instance showAnnotation :: Show Annotation where 
    show = genericShow 

instance showModifier :: Show Modifier where
   show Public = "public" 
   show Private = "private"
   show Protected = "protected"
   show Abstract = "abstract"
   show Final = "final"
   show Static = "static"
   show Transient = "transient"
   show (Annotation a) = show a
