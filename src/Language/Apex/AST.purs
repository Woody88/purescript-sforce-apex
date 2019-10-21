module Language.Apex.AST where

import Prelude
import Text.Parsing.Parser.Pos

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type Token = Tuple T Position

-- | Produced from 'Language.Java.Lexer'
data T  = Keyword    String
        | Operator   String
        -- | Tokens
        | TokInt     Int
        | TokLong    BigInt
        | TokDouble  Number 
        | TokString  String
        | TokBool    Boolean
        | TokIdent   String
        | TokNull
        -- | Separators.
        | LParen     | RParen
        | LBrace     | RBrace
        | LSquare    | RSquare
        | SemiColon  | Comma
        | Period     | TPeriod
        | At         

data Ident = Ident String

type TypeName = List Ident

data Type = PrimType PrimType
          | RefType RefType

data PrimType
    = BooleanT
    | ByteT
    | IntegerT
    | LongT
    | DecimalT
    | BlobT
    | DoubleT
    | DateT
    | DatetimeT 
    | IdT 
    | ObjectT 
    | StringT 
    | TimeT

data RefType = ClassOrInterfaceType ClassOrInterfaceType
             | TypeVariable TypeVariable
             | ArrayType ArrayType

type ClassOrInterfaceType = ClassType

data ClassType = ClassType TypeName (Maybe TypeArgs)


type InterfaceType = ClassType

type TypeVariable = Ident

data ArrayType = PrimArrayT PrimType Dims
               | ClassOrInterfaceArrayT ClassOrInterfaceType Dims
               | TypeVarArrayT TypeVariable Dims

type Dims = Int

type TypeArgs = TypeArgList

type TypeArgList = List TypeArg

data TypeArg = ActualType RefType

-- data TypeParam = TypeParam Ident (Maybe TypeBound)
--                PRODUCTION

-- data TypeBound = ExtendsTypeVar TypeVariable
--                | ExtendsClassType ClassOrInterfaceType [AdditionalBound]
--                PRODUCTION

-- type AdditionalBound = InterfaceType

-- data WildcardBound = SuperWB RefType
--                    | ExtendsWB RefType
--                    PRODUCTION

-- type TypeParams = [TypeParam]

-- -- | 7. Packages

-- data CompilationUnit = CompilationUnit (Maybe PackageDeclaration)
--                                 [ImportDeclaration] [TypeDeclaration]
--                      PRODUCTION

-- data PackageDeclaration = PackageDeclaration [PackageModifier] TypeName
--                         PRODUCTION

-- data PackageModifier = PackageModifier
--                      PRODUCTION

-- data ImportDeclaration = SingleTypeImportDeclaration TypeName
--                        | TypeImportOnDemandDeclaration TypeName
--                        | SingleStaticImportDeclaration TypeName Ident
--                        | StaticImportOnDemandDeclaration TypeName
--                        PRODUCTION

-- data TypeDeclaration = ClassDeclaration ClassDeclaration
--                      | InterfaceDeclaration InterfaceDeclaration
--                      | EmptyStatement
--                      PRODUCTION

-- -- | 8. Classes

-- data SuperClass = Extends ClassType
--                 PRODUCTION

-- type InterfaceTypeList = [ClassType]
-- data SuperInterfaces = Implements InterfaceTypeList
--                      PRODUCTION

-- data ClassDeclaration = Class [ClassModifier] Ident
--                       (Maybe [TypeParam]) (Maybe SuperClass)
--                       (Maybe SuperInterfaces) ClassBody
--                       | Enum [ClassModifier]
--                         Ident (Maybe SuperInterfaces) EnumBody
--                       PRODUCTION

-- data Modifier = Public    | Protected    | Private   | Abstract
--               | Static    | Final        | StrictFP  | Volatile
--               | Transient | Synchronized | Native    | Interface
--               | Default
--               PRODUCTION

-- type ClassModifier = Modifier

-- type ClassBody = [ClassBodyDeclaration]

-- data ClassBodyDeclaration = ClassMemberDeclaration ClassMemberDeclaration
--                           | InstanceInitializer Block
--                           | StaticInitializer Block
--                           | ConstructorDeclaration [ConstructorModifier]
--                                 ConstructorDeclarator (Maybe Throws)
--                                 ConstructorBody
--                           PRODUCTION

-- type UnannType = Type

-- data ClassMemberDeclaration =
--             FieldDeclaration  [FieldModifier] UnannType VariableDeclaratorList
--           | MethodDeclaration [MethodModifier] MethodHeader MethodBody
--           | MemberClassDeclaration ClassDeclaration
--           | MemberInterfaceDeclaration InterfaceDeclaration
--           | EmptyClassMember
--           PRODUCTION

-- type FieldModifier = Modifier

-- type VariableDeclaratorList = [VariableDeclarator]

-- data VariableDeclarator =
--         VariableDeclarator VariableDeclID (Maybe VariableInitializer)
--         PRODUCTION

-- type VariableDeclID = (Ident, Dims)

-- type MethodModifier = Modifier

-- data Result = RType UnannType
--             | RVoid
--             PRODUCTION

-- data MethodHeader = MethodHeader Result MethodDeclarator (Maybe Throws)
--                   | MethodHeaderTP TypeParams Result MethodDeclarator
--                     (Maybe Throws)
--                   PRODUCTION

-- data MethodDeclarator = MethodDeclarator Ident
--                       (Maybe FormalParameterList) Dims
--                       PRODUCTION

-- data Throws = Throws ExceptionTypeList
--             PRODUCTION

-- type ExceptionTypeList = [ExceptionType]

-- data ExceptionType = ClassTypeEx ClassType
--                    | TypeVariableEx TypeVariable
--                    PRODUCTION

-- data MethodBody = MethodBody Block
--                 | EmptyBody
--                 PRODUCTION

-- type InstanceInitializer = Block
-- type StaticInitializer = Block

-- type FormalParameterList = [FormalParameter]

-- data FormalParameter =
--              FormalParameter
--                 [VariableModifier] UnannType VariableDeclID
--             -- TODO Change VariableModifier to Annotation
--              | ReceiverParameter
--                 [VariableModifier] UnannType (Maybe Ident)
--             -- TODO Change VariableModifier to Annotation
--              | EllipsisParameter
--                 [VariableModifier] UnannType [VariableModifier] VariableDeclID
--              PRODUCTION

-- -- TODO Add Annotation
-- data VariableModifier = FinalV
--                       | VoidV
--                       PRODUCTION

-- data VariableInitializer = ExpressionInitializer Expression
--                          | ArrayInitializer ArrayInitializer
--                          PRODUCTION

-- -- data ConstructorDeclaration (See ClassMemberDec)

-- type ConstructorModifier = Modifier

-- data ConstructorDeclarator = ConstructorDeclarator (Maybe TypeParams)
--                            SimpleTypeName (Maybe FormalParameterList)
--                            PRODUCTION

-- type SimpleTypeName = Ident

-- data ConstructorBody = ConstructorBody
--                      (Maybe ExplicitConstructorInvocation)
--                      (Maybe BlockStatements)
--                      PRODUCTION

-- data ExplicitConstructorInvocation =
--                     ThisECI (Maybe TypeArgs) (Maybe ArgList)
--                   | SuperECI (Maybe TypeArgs) (Maybe ArgList)
--                   | NameSuperECI TypeName (Maybe TypeArgs) (Maybe ArgList)
--                   | PrimarySuperECI Primary (Maybe TypeArgs) (Maybe ArgList)
--                   PRODUCTION

-- data EnumBody = EnumBody (Maybe EnumConstantList) (Maybe EnumBodyDeclarations)
--               PRODUCTION

-- data EnumConstant = EnumConstant [EnumConstantModifier] Ident
--                         (Maybe ArgList) (Maybe ClassBody)
--                   PRODUCTION

-- type EnumConstantList = [EnumConstant]

-- type EnumBodyDeclarations = [ClassBodyDeclaration]

-- -- | TODO Change to annotation
-- data EnumConstantModifier = EnumConstantModifier
--                           PRODUCTION

-- -- | 9. Interfaces
-- data InterfaceDeclaration = NormalInterface [InterfaceModifier] Ident
--                             (Maybe TypeParams) (Maybe ExtendsInterfaces)
--                              InterfaceBody
--                           PRODUCTION

-- type InterfaceModifier = Modifier

-- data ExtendsInterfaces = ExtendsInterfaces InterfaceTypeList
--                        PRODUCTION

-- type InterfaceBody = [InterfaceMemberDeclaration]

-- data InterfaceMemberDeclaration =
--           ConstantDeclaration [ConstantModifier] UnannType
--            VariableDeclaratorList
--         | InterfaceMethodDeclaration [InterfaceMethodModifier]
--             MethodHeader MethodBody
--         | InterfaceClassDeclaration ClassDeclaration
--         | InnerInterfaceDeclaration InterfaceDeclaration
--         PRODUCTION

-- type ConstantModifier = Modifier
-- type InterfaceMethodModifier = Modifier

-- -- | 14. Blocks and Statements
-- data Block = Block BlockStatements
--            | EmptyBlock
--            PRODUCTION

-- type BlockStatements = [BlockStatement]

-- data BlockStatement = LocalVariableDeclarationStmt LocalVariableDeclaration
--                     | ClassDeclarationStmt ClassDeclaration
--                     | Statement Statement
--                     PRODUCTION

-- data LocalVariableDeclaration = LocalVariableDeclaration [VariableModifier]
--                               UnannType VariableDeclaratorList
--                               PRODUCTION

-- data Statement = StatementWTS StatementWTS
--                | LabeledStmt Ident Statement
--                | IfThenStmt Expression Statement
--                | IfThenElseStmt Expression StatementNSI Statement
--                | WhileStmt Expression Statement
--                | ForStmt ForStatement
--                PRODUCTION

-- data StatementNSI = StatementWTSNSI StatementWTS
--                   | LabeledStmtNSI Ident StatementNSI
--                   | IfThenElseStmtNSI Expression StatementNSI StatementNSI
--                   | WhileStmtNSI Expression StatementNSI
--                   | ForStmtNSI ForStatementNSI
--                   PRODUCTION

-- data StatementWTS = BlockStmt Block
--                   | EmptyStmt
--                   | ExpressionStmt StatementExpression
--                   | AssertStmt Expression
--                   | AssertLblStmt Expression Expression
--                   | SwitchStmt Expression SwitchBlock
--                   | DoStmt Statement Expression
--                   | BreakStmt (Maybe Ident)
--                   | ContinueStmt (Maybe Ident)
--                   | ReturnStmt (Maybe Expression)
--                   | SynchronizedStmt Expression Block
--                   | ThrowStmt Expression
--                   | TryStmt TryStmt
--                   PRODUCTION

-- data SwitchBlock = SwitchBlock [SwitchBlockStmtGrp] [SwitchLabel]
--                  PRODUCTION

-- data SwitchBlockStmtGrp = SwitchBlockStmtGrp SwitchLabels BlockStatements
--                         PRODUCTION

-- type StatementExpression = AssignmentExpression

-- data ForInit = ForInitExpr StatementExpressionList
--              | ForInitDecl LocalVariableDeclaration
--              PRODUCTION

-- type StatementExpressionList = [StatementExpression]
-- type ForUpdate = StatementExpressionList
-- type SwitchLabels = [SwitchLabel]

-- data SwitchLabel = CaseExpr ConstantExpression
--                  | CaseEnum EnumConstantName
--                  | CaseDefault
--                  PRODUCTION

-- type EnumConstantName = Ident

-- data ForStatement = BasicFor (Maybe ForInit)
--                         (Maybe Expression) (Maybe ForUpdate) Statement
--                   | EnhancedFor [VariableModifier] UnannType VariableDeclID
--                         Expression Statement
--                   PRODUCTION

-- data ForStatementNSI = BasicForNSI (Maybe ForInit)
--                         (Maybe Expression) (Maybe ForUpdate) StatementNSI
--                   | EnhancedForNSI [VariableModifier] UnannType VariableDeclID
--                         Expression StatementNSI
--                      PRODUCTION

-- data TryStmt = TryCatch Block Catches
--              | TryFinally Block (Maybe Catches) Finally
--              | TryWithResources ResourceSpecification Block
--                 (Maybe Catches) (Maybe Finally)
--              PRODUCTION

-- type Finally = Block
-- type Catches = [CatchClause]

-- data CatchClause = CatchClause CatchFormalParameter Block
--                  PRODUCTION

-- data CatchFormalParameter = CatchFormalParameter [VariableModifier] CatchType
--                           VariableDeclID
--                           PRODUCTION

-- type CatchType = [ClassType]
-- type ResourceSpecification = ResourceList
-- type ResourceList = [Resource]
-- data Resource = Resource [VariableModifier] UnannType VariableDeclID Expression
--               PRODUCTION

-- -- | 15. Expressions
-- data Expression = LambdaExpression LambdaParameters LambdaBody
--                 | Expression AssignmentExpression
--                 PRODUCTION

-- data AssignmentExpression = Term Term
--                           | Assignment LHS T Expression
--                           PRODUCTION

-- -- | Literals [literal]
-- data Literal = IntegerLiteral Integer
--              | FloatingPointLiteral String
--              | BooleanLiteral Bool
--              | CharacterLiteral Char
--              | StringLiteral String
--              | NullLiteral
--              PRODUCTION

-- -- | Primary [primary]
-- data Primary = Literal Literal
--                 -- | foo.class
--                 | TypeNameDotClass TypeName
--                 -- | foo[][].class
--                 | TypeNameArrDotClass TypeName Int
--                 -- | void.class
--                 | VoidDotClass
--                 -- | this
--                 | This
--                 -- | foo.this
--                 | TypeNameDotThis TypeName
--                 -- | (Expression)
--                 | Expr Expression
--                 -- | Instant class creation
--                 | ClassInstanceCreationExpression ClassInstanceCreation
--                 -- | Field access
--                 | FieldAccess FieldAccess
--                 -- | Array access
--                 | ArrayAccess ArrayAccess
--                 -- | Method invocation
--                 | MethodInvocation MethodInvocation
--                 -- | Method reference
--                 | MethodReference MethodReference
--                 -- | Array creation expression
--                 | ArrayCreation ArrayCreationExpr
--                 -- | Composition of two or more primaries.
--                 | Dot Primary Primary
--                 PRODUCTION

-- -- | Class instance creation [classInstanceCreation]
-- --  e.g  new Comparable<String> {
-- --             <Class Body>
-- --  }
-- data ClassInstanceCreation =
--        WithIdentifier (Maybe TypeArgs) Ident
--                 (Maybe TypeArgsOrDiam) (Maybe ArgList) (Maybe ClassBody)
--      | WithExpressionName TypeName (Maybe TypeArgs)
--                 (Maybe TypeArgsOrDiam) (Maybe ArgList) (Maybe ClassBody)
--      | WithPrimary Primary (Maybe TypeArgs) Ident
--                 (Maybe TypeArgsOrDiam) (Maybe ArgList) (Maybe ClassBody)
--      PRODUCTION

-- data TypeArgsOrDiam = TypeArgs TypeArgs
--                     | Diamond
--                     PRODUCTION

-- -- | Field Access [fieldAccess]
-- --  Style 1 <expression>.field
-- --  Style 2 super.field
-- --  Style 3 <expression>.super.field
-- data FieldAccess = ExprFieldAccess Primary Ident
--                  | SelfParentFieldAccess Ident
--                  | ParentFieldAccess TypeName Ident
--                  PRODUCTION

-- -- | Array Access
-- -- Style 1 <name>[<expression>]
-- -- Style 2 <primary>[<expression>]
-- data ArrayAccess = NormalArrayAccess TypeName Expression
--                  | ExprArrayAccess Primary Expression
--                  PRODUCTION

-- data MethodInvocation =
--           NormalMethodInvocation TypeName (Maybe ArgList)
--         | NameMethodInvocation TypeName (Maybe TypeArgs) Ident (Maybe ArgList)
--         | ExprMethodInvocation Primary (Maybe TypeArgs) Ident (Maybe ArgList)
--         | SelfParentMethodInvocation (Maybe TypeArgs) Ident (Maybe ArgList)
--         | ParentMethodInvocation TypeName (Maybe TypeArgs) Ident (Maybe ArgList)
--         PRODUCTION

-- data ArgList = ArgList [Expression]
--                PRODUCTION

-- data MethodReference = NameMR TypeName (Maybe TypeArgs) Ident
--                      | RefTypeMR RefType (Maybe TypeArgs) Ident
--                      | ExprMR  Primary (Maybe TypeArgs) Ident
--                      | SelfParentMR (Maybe TypeArgs) Ident
--                      | ParentMR TypeName (Maybe TypeArgs) Ident
--                      | ClassTypeMR ClassType (Maybe TypeArgs)
--                      | ArrayTypeMR ArrayType
--                      PRODUCTION

-- data DimExpr = DimExpr (Maybe Expression)
--              PRODUCTION

-- type DimExprs = [DimExpr]

-- data ArrayCreationExpr =
--          PrimTypeACE PrimType DimExprs Int
--        | ClassTypeACE ClassType DimExprs Int
--        | PrimTypeACEI PrimType Int ArrayInitializer
--        | ClassTypeACEI ClassType Int ArrayInitializer
--        PRODUCTION

-- type ArrayInitializer = [VariableInitializer]

-- type ConstantExpression = Expression

-- data LambdaParameters = LIdent Ident
--                       | LFormalParameterList (Maybe FormalParameterList)
--                       | LInferredFormalParameterList InferredFormalParameterList
--                       PRODUCTION

-- type InferredFormalParameterList = [Ident]

-- data LambdaBody = Lambda Expression
--                 | LambdaBlock Block
--                 PRODUCTION

-- data LHS = LHSExpr  Primary
--          | LHSIdent Ident
--          PRODUCTION

-- data PostfixExpr = PrimPostfixExpr Primary
--                  | NamePostfixExpr TypeName
--                  | PostIncrementExpr PostfixExpr
--                  | PostDecrementExpr PostfixExpr
--                  PRODUCTION

-- data UnaryExpression = PreIncrementExpr UnaryExpression
--                      | PreDecrementExpr UnaryExpression
--                      | UnaryPlus UnaryExpression
--                      | UnaryMinus UnaryExpression
--                      | UnaryNPM UnaryExpressionNotPlusMinus
--                      PRODUCTION

-- data UnaryExpressionNotPlusMinus = UnaryPostfix PostfixExpr
--                                  | UnaryTilde UnaryExpression
--                                  | UnaryNegate UnaryExpression
--                                  | UnaryCast CastExpression
--                                  PRODUCTION

-- data CastExpression = UnaryToPrim PrimType UnaryExpression
--                     | UnaryToRef RefType [ClassType] UnaryExpressionNotPlusMinus
--                     | LambdaToRef RefType [ClassType] Expression
--                     PRODUCTION

-- data CondExpr = CondExpr Expression Expression Expression
--               PRODUCTION

-- data Term = PrimExpr Primary
--           | NameExpr TypeName
--           | PrefixExpr T Term
--           | PostfixExpr T Term
--           | InstanceOfExpr RefType Term
--           | BinaryExpr T Term Term
--           | ConditionalExpr Expression Term Term
--           PRODUCTION

derive instance genericT :: Generic T _ 

instance showT :: Show T where 
    show = genericShow