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
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (Maybe Type)
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass Name
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation (List TypeArgument) ClassType (List Argument) (Maybe ClassBody)
    -- | The application of a binary operator to two operand expressions.
    | BinOp Exp Op Exp
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate Type (List Exp) Int
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit Type Int ArrayInit
    -- | Assignment of the result of an expression to a variable.
    | Assign Lhs AssignOp Exp
    -- | An expression name, e.g. a variable.
    | ExpName Name
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement Exp
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement Exp
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  Exp
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  Exp
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  Exp
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus Exp
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl Exp
    -- | Logical complementation of boolean values.
    | PreNot  Exp
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast  Type Exp
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation Exp (List TypeArgument) Ident (List Argument) (Maybe ClassBody)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess ArrayIndex
    -- | A method invocation expression.
    | MethodInv MethodInvocation
    -- | A field access expression.
    | FieldAccess FieldAccess
    -- | Method reference
    | MethodRef Name Ident
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond Exp Exp Exp
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf Exp RefType

    
-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs
    = NameLhs Name          -- ^ Assign to a variable
    | FieldLhs FieldAccess  -- ^ Assign through a field access
    | ArrayLhs ArrayIndex   -- ^ Assign to an array

-- | Array access
data ArrayIndex = ArrayIndex Exp (List Exp)    -- ^ Index into an array

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess
    = PrimaryFieldAccess Exp Ident      -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess Ident            -- ^ Accessing a field of the superclass.
    | ClassFieldAccess Name Ident       -- ^ Accessing a (static) field of a named class.

-- | A Java statement.
data Stmt
    -- | A statement can be a nested block.
    = StmtBlock Block
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen Exp Stmt
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse Exp Stmt Stmt
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While Exp Stmt
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (Maybe ForInit) (Maybe Exp) (Maybe (List Exp)) Stmt
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor (List Modifier) Type Ident Exp Stmt
    -- | An empty statement does nothing.
    | Empty
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt Exp
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert Exp (Maybe Exp)
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch Exp (List SwitchBlock)
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do Stmt Exp
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (Maybe Ident)
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue (Maybe Ident)
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (Maybe Exp)
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw Exp
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try Block (List Catch) (Maybe {- finally -} Block)
    -- | Statements may have label prefixes.
    | Labeled Ident Stmt
  
-- | Initialization code for a basic @for@ statement.
data ForInit
    = ForLocalVars (List Modifier) Type (List VarDecl)
    | ForInitExps (List Exp)

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch = Catch FormalParam Block

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock
    = SwitchBlock SwitchLabel (List BlockStmt)

-- | A label within a @switch@ statement.
data SwitchLabel
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase Exp
    | Default

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
    | LocalClass ClassDecl
    | BlockStmt Stmt

derive instance genericBlock :: Generic Block _ 
derive instance genericBlockStmt :: Generic BlockStmt _ 

derive instance eqBlockStmt :: Eq BlockStmt
derive instance eqBlock :: Eq Block

instance showBlockStmt :: Show BlockStmt where
    show (BlockStmt st) = "(BlockStmt " <> show st <> ")"
    show x = genericShow x

instance showBlock :: Show Block where
    show = genericShow

newtype CompilationUnit = CompilationUnit (List TypeDecl)

----------------------- Declation Types -----------------------------------
-- | A type declaration declares a class type or an interface type.
data TypeDecl
    = ClassTypeDecl ClassDecl
    | InterfaceTypeDecl InterfaceDecl

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
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl InterfaceDecl

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
    | With_Share
    | Without_Share
    | Inherit_Share
    | Override
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

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation
    -- | Invoking a specific named method.
    = MethodCall Name (List Argument)
    -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall Exp (List RefType) Ident (List Argument)
    -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall (List RefType) Ident (List Argument)
    -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    | ClassMethodCall Name (List RefType) Ident (List Argument)
    -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    | TypeMethodCall  Name (List RefType) Ident (List Argument)

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
derive instance genericTypeDecl :: Generic TypeDecl _
derive instance genericCompilationUnit :: Generic CompilationUnit _
derive instance genericTuple3 :: Generic (Tuple3 a b c) _
derive instance genericArrayIndex :: Generic ArrayIndex _
derive instance genericFieldAccess :: Generic FieldAccess _
derive instance genericLhs :: Generic Lhs _
derive instance genericAssignOp :: Generic AssignOp _
derive instance genericMethodInvocation :: Generic MethodInvocation _
derive instance genericSwitchLabel :: Generic SwitchLabel _
derive instance genericSwitchBlock :: Generic SwitchBlock _
derive instance genericCatch :: Generic Catch _
derive instance genericForInit :: Generic ForInit _
derive instance genericStmt :: Generic Stmt _


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
derive instance eqTypeDecl :: Eq TypeDecl 
derive instance eqCompilationUnit :: Eq CompilationUnit 
derive instance eqArrayIndex :: Eq ArrayIndex 
derive instance eqFieldAccess :: Eq FieldAccess 
derive instance eqLhs :: Eq Lhs 
derive instance eqAssignOp :: Eq AssignOp 
derive instance eqMethodInvocation :: Eq MethodInvocation 
derive instance eqSwitchLabel :: Eq SwitchLabel 
derive instance eqSwitchBlock :: Eq SwitchBlock 
derive instance eqCatch :: Eq Catch 
derive instance eqForInit :: Eq ForInit 
derive instance eqStmt :: Eq Stmt 

instance showStmt ::  Show Stmt where 
    show (Try b lc mb) = "(Try " <> show b <> show lc <> show mb <> ")"
    show x = genericShow x 

instance showForInit ::  Show ForInit where 
    show = genericShow

instance showCatch ::  Show Catch where 
    show (Catch fp b) = "(Catch " <> show fp <> show b <> ")"

instance showSwitchBlock ::  Show SwitchBlock where 
    show (SwitchBlock sl lb) = "(SwitchBlock " <> show sl <> show lb <> ")"

instance showSwitchLabel ::  Show SwitchLabel where 
    show = genericShow

instance showMethodInvocation ::  Show MethodInvocation where 
    show = genericShow

instance showAssignOp ::  Show AssignOp where 
    show = genericShow

instance showLhs ::  Show Lhs where 
    show = genericShow

instance showFieldAccess ::  Show FieldAccess where 
    show = genericShow

instance showArrayIndex ::  Show ArrayIndex where 
    show = genericShow

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where 
    show = genericShow

instance showCompilationUnit :: Show CompilationUnit where 
    show = genericShow

instance showTypeDecl :: Show TypeDecl where 
    show = genericShow

instance showInterfaceDecl :: Show InterfaceDecl where 
    show = genericShow

instance showInterfaceBody :: Show InterfaceBody where 
    show (InterfaceBody b) = "(InterfaceBody " <> show b <> ")"

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
        (BinOp x op y) -> "(BinOp " <> show x <> " " <> show op <> " " <> show y <> ")"
        _ -> genericShow exp

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
   show With_Share = "with sharing"
   show Without_Share = "without sharing"
   show Inherit_Share = "inherit sharing"
   show Override = "override"
   show (Annotation a) = show a
