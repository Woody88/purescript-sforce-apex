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

-- | A class declaration specifies a new named reference type.
data ClassDecl
    = ClassDecl (List Modifier) Ident (List TypeParam) (Maybe RefType) (List RefType) ClassBody
    | EnumDecl  (List Modifier) Ident (List RefType) EnumBody

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody = ClassBody (List Decl)

-- | The body of an enum type may contain enum constants.
data EnumBody = EnumBody (List EnumConstant) (List Decl)

-- | An enum constant defines an instance of the enum type.
data EnumConstant = EnumConstant Ident (List Argument) (Maybe ClassBody)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl
    = InterfaceDecl (List Modifier) Ident (List TypeParam) (List RefType) InterfaceBody

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody
    = InterfaceBody (List MemberDecl)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl
    = MemberDecl MemberDecl
    | InitDecl Boolean Block

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl (List Modifier) Type (List VarDecl)
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl (List Modifier) (List TypeParam) (Maybe Type) Ident (List FormalParam) (Maybe Exp) MethodBody
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl (List Modifier) (List TypeParam) Ident (List FormalParam) ConstructorBody
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl ClassDecl
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

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody = ConstructorBody (Maybe ExplConstrInv) (List BlockStmt)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv
    = ThisInvoke             (List RefType) (List Argument)
    | SuperInvoke            (List RefType) (List Argument)
    | PrimarySuperInvoke Exp (List RefType) (List Argument)

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
derive instance genericConstructorBody :: Generic ConstructorBody _
derive instance genericDecl :: Generic Decl _
derive instance genericClassDecl :: Generic ClassDecl _
derive instance genericClassBody :: Generic ClassBody _
derive instance genericEnumBody :: Generic EnumBody _
derive instance genericEnumConstant :: Generic EnumConstant _
derive instance genericInterfaceDecl :: Generic InterfaceDecl _
derive instance genericInterfaceBody :: Generic InterfaceBody _



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
derive instance eqExplConstrInv :: Eq ExplConstrInv
derive instance eqConstructorBody :: Eq ConstructorBody 
derive instance eqDecl :: Eq Decl 
derive instance eqClassDecl :: Eq ClassDecl 
derive instance eqClassBody :: Eq ClassBody 
derive instance eqEnumBody :: Eq EnumBody 
derive instance eqEnumConstant :: Eq EnumConstant 
derive instance eqInterfaceDecl :: Eq InterfaceDecl 
derive instance eqInterfaceBody :: Eq InterfaceBody 

instance showInterfaceDecl :: Show InterfaceDecl where 
    show = genericShow

instance showInterfaceBody :: Show InterfaceBody where 
    show = genericShow

instance showClassBody :: Show ClassBody where 
    show = genericShow

instance showEnumBody :: Show EnumBody where 
    show = genericShow

instance showEnumConstant :: Show EnumConstant where 
    show = genericShow

instance showClassDecl :: Show ClassDecl where 
    show = genericShow

instance showDecl :: Show Decl where 
    show (MemberDecl m) = "(MemberDecl " <> show m <> ")"
    show (InitDecl bool bl) = "(InitDecl " <> show bool <> " " <> show bl <> ")"

instance showConstructorBody :: Show ConstructorBody where 
    show = genericShow

instance showConstrInv :: Show ExplConstrInv where 
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
