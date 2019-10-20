module Language.Apex.Parser where 


import Data.Either
import Language.Apex.Lexer
import Language.Apex.Syntax
import Language.Apex.Syntax.Types
import Prelude
import Text.Parsing.Parser.Pos
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Lazy (fix)
import Control.Monad.State (gets, modify_)
import Data.Foldable (foldl)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Language.Apex.Lexer.Types (L(..), Token(..))
import Text.Parsing.Parser (ParseState(..), Parser, ParseError, position, runParser, fail)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

type P = Parser (List (L Token))

------------- Top Level parsing -----------------

parseCompilationUnit :: String -> Either ParseError CompilationUnit
parseCompilationUnit input = runParser (lexJava input) compilationUnit

------------- Compilation Unit -----------------
compilationUnit :: P CompilationUnit
compilationUnit = do 
    tds <- list1 typeDecl
    pure $ CompilationUnit (List.catMaybes tds)

literal :: P Literal
literal = javaToken $ \t -> case t of
    IntegerTok i -> Just (Integer i)
    LongTok    l -> Just (Long l)
    DoubleTok  d -> Just (Double d)
    StringTok  s -> Just (String s)
    BoolTok    b -> Just (Boolean b)
    NullTok      -> Just Null 
    _ -> Nothing

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

fieldDecl :: P (Mod MemberDecl)
fieldDecl = endSemi $ do
    typ <- type_
    vds <- varDecls
    pure $ \ms -> FieldDecl ms typ vds

methodDecl :: P (Mod MemberDecl)
methodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    bod <- methodBody
    pure $ \ms -> MethodDecl ms tps rt id fps Nothing bod

methodBody :: P MethodBody
methodBody = MethodBody <$>
    (const Nothing <$> semiColon <|> Just <$> (fix $ \_ -> block))

-- Formal parameters

formalParams :: P (List FormalParam)
formalParams = parens $ seplist (fix $ \_ -> formalParam) comma

formalParam :: P FormalParam
formalParam = do
    ms  <- list (fix $ \_ -> modifier)
    typ <- type_
    vid <- varDeclId
    pure $ FormalParam ms typ vid


-- Declations 
typeDecl :: P (Maybe TypeDecl)
typeDecl = Just <$> classOrInterfaceDecl <|> const Nothing <$> semiColon

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = do
    ms <- list (fix $ \_ -> modifier)
    de <- (do cd <- classDecl
              pure $ \ms -> ClassTypeDecl (cd ms)) <|>
          (do id <- interfaceDecl
              pure $ \ms -> InterfaceTypeDecl (id ms))
    pure $ de ms

classDecl :: P (Mod ClassDecl)
classDecl = fix $ \_ -> normalClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod ClassDecl)
normalClassDecl = do 
    tok KW_Class 
    i    <- ident 
    tps  <- lopt typeParams
    ext  <- optMaybe extends 
    imp  <- lopt implements
    body <- classBody
    pure $ \ms -> ClassDecl ms i tps (ext >>= List.head) imp body

enumClassDecl :: P (Mod ClassDecl)
enumClassDecl = do
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    pure $ \ms -> EnumDecl ms i imp bod

enumBodyDecls :: P (List Decl)
enumBodyDecls = fix $ \_ -> semiColon *> classBodyStatements

enumBody :: P EnumBody
enumBody = fix $ \_ -> braces $ do
    ecs <- seplist enumConst comma
    PC.optional comma
    eds <- lopt enumBodyDecls
    pure $ EnumBody ecs eds

enumConst :: P EnumConstant
enumConst = do
    id  <- ident
    as  <- lopt args
    mcb <- optMaybe classBody
    pure $ EnumConstant id as mcb

classBody :: P ClassBody 
classBody = fix $ \_ -> ClassBody <$> braces classBodyStatements

classBodyStatements :: P (List Decl)
classBodyStatements = fix $ \_ -> List.catMaybes <$> list classBodyStatement

classBodyStatement :: P (Maybe Decl)
classBodyStatement =
    (PC.try $ do
       _ <- list1 semiColon
       pure Nothing) <|>
    (PC.try $ do
       mst <- bopt (tok KW_Static)
       blk <- block
       pure $ Just $ InitDecl mst blk) <|>
    (PC.try $ do 
        ms  <- list (fix $ \_ -> modifier)
        dec <- memberDecl
        pure $ Just $ MemberDecl (dec ms))

memberDecl :: P (Mod MemberDecl)
memberDecl = fix $ \_ -> 
    (PC.try $ do
        cd  <- classDecl
        pure $ \ms -> MemberClassDecl (cd ms)) <|>
    (PC.try $ do
        id  <- interfaceDecl
        pure $ \ms -> MemberInterfaceDecl (id ms)) <|>
    PC.try fieldDecl <|>
    PC.try methodDecl <|>
    constrDecl
    
constrDecl :: P (Mod MemberDecl)
constrDecl = do
    tps <- lopt typeParams
    id  <- ident
    fps <- formalParams
    bod <- constrBody
    pure $ \ms -> ConstructorDecl ms tps id fps bod

constrBody :: P ConstructorBody
constrBody = braces $ do
    mec <- optMaybe (PC.try $ (fix $ \_ -> explConstrInv))
    bss <- list blockStmt
    pure $ ConstructorBody mec bss

explConstrInv :: P ExplConstrInv
explConstrInv = endSemi $
    (PC.try $ do
        tas <- lopt refTypeArgs
        tok KW_This
        as  <- args
        pure $ ThisInvoke tas as) <|>
    (PC.try $ do
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        pure $ SuperInvoke tas as) <|>
    (do pri <- fix $ \_ -> primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        pure $ PrimarySuperInvoke pri tas as)

args :: P (List Argument)
args = parens $ seplist (fix $ \_ -> expression) comma

-- Interface Declaration 

interfaceDecl :: P (Mod InterfaceDecl)
interfaceDecl = do
    tok KW_Interface
    id  <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    pure $ \ms -> InterfaceDecl ms id tps exs bod

interfaceBody :: P InterfaceBody
interfaceBody = fix $ \_ -> InterfaceBody <<< List.catMaybes <$> braces (list interfaceBodyDecl)

interfaceBodyDecl :: P (Maybe MemberDecl)
interfaceBodyDecl = semiColon *> pure Nothing <|>
    do ms  <- list $ fix $ \_ -> modifier
       imd <- interfaceMemberDecl
       pure $ Just (imd ms)

interfaceMemberDecl :: P (Mod MemberDecl)
interfaceMemberDecl = fix $ \_ -> 
    (do cd  <- classDecl
        pure $ \ms -> MemberClassDecl (cd ms)) <|>
    (do id  <- PC.try interfaceDecl
        pure $ \ms -> MemberInterfaceDecl (id ms)) <|>
    fieldDecl

-- Modifiers

modifier :: P Modifier
modifier =
        tok KW_Static        *> pure Static
    <|> tok KW_Public        *> pure Public
    <|> tok KW_Protected     *> pure Protected
    <|> tok KW_Private       *> pure Private
    <|> tok KW_Abstract      *> pure Abstract
    <|> tok KW_Final         *> pure Final
    <|> tok KW_Transient     *> pure Transient
    <|> tok KW_With_Share    *> pure With_Share
    <|> tok KW_Without_Share *> pure Without_Share
    <|> tok KW_Inherit_Share *> pure Inherit_Share
    <|> tok KW_Override      *> pure Override 
    <|> Annotation <$> (fix $ \_ -> annotation)

annotation :: P Annotation
annotation = do 
    annName <- tok Op_AtSign *> name
    PC.try (parens evlist >>= \annKV -> pure $ NormalAnnotation { annName,  annKV }) <|> (pure $ MarkerAnnotation { annName })


evlist :: P (List (Tuple Ident ElementValue))
evlist = fix $ \_ -> seplist1 elementValuePair comma

elementValuePair :: P (Tuple Ident ElementValue)
elementValuePair = fix $ \_ -> Tuple <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P ElementValue
elementValue = fix $ \_ -> EVVal <$> InitExp <$> infixExp

------------ Variable declarations ----------------

localVarDecl :: P (Tuple3 (List Modifier) Type (List VarDecl))
localVarDecl = do
    ms <- list (fix $ \_ -> modifier)
    typ <- type_
    vds <- varDecls
    pure $ Tuple3 ms typ vds

varDecls :: P (List VarDecl)
varDecls = fix $ \_ -> seplist1 varDecl comma 

varDecl :: P VarDecl 
varDecl = do
    vdi <- varDeclId
    vi  <- optMaybe $ tok Op_Equal *> varInit 
    pure $ VarDecl vdi vi 

varDeclId :: P VarDeclId 
varDeclId = do 
    id <- ident
    abs <- list arrBrackets
    pure $ foldl (\f _ -> VarDeclArray <<< f) VarId abs id

varInit :: P VarInit
varInit = fix $ \_ -> InitArray <$> arrayInit <|> InitExp <$> expression 

arrayInit :: P ArrayInit
arrayInit = fix $ \_ -> braces $ do
    vis <- seplist varInit comma
    _ <- optMaybe comma
    pure $ ArrayInit vis

------------ Expression ---------------
-- more to be added
expression :: P Exp
expression = fix $ \_ -> infixExp 

stmtExp :: P Exp
stmtExp = PC.try preIncDec
    <|> PC.try postIncDec
    <|> PC.try assignment
    <|> PC.try methodInvocationExp
    <|> PC.try methodRef
    <|> instanceCreation

preIncDec :: P Exp
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    pure $ op e

postIncDec :: P Exp
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    pure $ foldl (\a s -> s a) e ops

assignment :: P Exp
assignment = do
    lh <- lhs
    op <- assignOp
    e  <- expression
    pure $ Assign lh op e

lhs :: P Lhs
lhs = PC.try (FieldLhs <$> fieldAccess)
    <|> PC.try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name

methodInvocationSuffix :: P (Exp -> MethodInvocation)
methodInvocationSuffix = do
    period
    rts <- lopt refTypeArgs
    i   <- ident
    as  <- args
    pure $ \p -> PrimaryMethodCall p mempty i as

methodInvocationNPS :: P MethodInvocation
methodInvocationNPS =
    (do tok KW_Super *> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        pure $ SuperMethodCall rts i as) <|>
    (do n <- name
        f <- (do 
                as <- args
                pure $ \n -> MethodCall n as) <|>
             (period *> do
                msp <- optMaybe (tok KW_Super *> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                pure $ \n -> mc n rts i as)
        pure $ f n)

methodInvocationExp :: P Exp
methodInvocationExp = 
    PC.try 
    (do
        p <- primaryNPS
        ss <- list primarySuffix
        let mip = foldl (\a s -> s a) p ss
        case mip of
            MethodInv _ -> pure mip
            _ -> fail "") <|>
     (MethodInv <$> methodInvocationNPS)

instanceCreationNPS :: P Exp
instanceCreationNPS = do 
    tok KW_New
    tas <- lopt typeArgs
    ct  <- classType
    as  <- args
    mcb <- optMaybe classBody
    pure $ InstanceCreation tas ct as mcb

instanceCreation :: P Exp
instanceCreation = 
    PC.try instanceCreationNPS <|> 
    (do
        p <- primaryNPS
        ss <- list primarySuffix
        let icp = foldl (\a s -> s a) p ss
        case icp of
            (QualInstanceCreation _ _ _ _ _) -> pure icp
            _ -> fail "")

primary :: P Exp 
primary = fix $ \_ -> primaryNPS |>> primarySuffix

    -- flip PC.withErrorMessage "primary expression" $ PC.choice $ map PC.try 
    --     [ (Lit <$> literal)
    --     , const This <$> tok KW_This 
    --     , fix $ \_ -> arrayCreation
    --     ]

primaryNPS :: P Exp
primaryNPS = fix $ \_ -> PC.try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArrayNPS :: P Exp
primaryNoNewArrayNPS = fix $ \_ -> 
    Lit <$> literal <|>
    const This <$> tok KW_This <|>
    parens (fix $ \_ -> expression) <|>
    -- TODO: These two following should probably be merged more
    (PC.try $ do
        rt <- resultType
        period *> tok KW_Class
        pure $ ClassLit rt) <|>
    (PC.try $ do
        n <- name
        period *> tok KW_This
        pure $ ThisClass n) <|>
    PC.try instanceCreationNPS <|>
    PC.try (MethodInv <$> methodInvocationNPS) <|>
    PC.try (FieldAccess <$> fieldAccessNPS) <|>
    ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp -> Exp)
primarySuffix = fix $ \_ -> 
    PC.try instanceCreationSuffix <|>
    PC.try (((<<<) ArrayAccess) <$> arrayAccessSuffix) <|>
    PC.try (((<<<) MethodInv)  <$> methodInvocationSuffix) <|>
    ((<<<) FieldAccess) <$> fieldAccessSuffix

startSuff :: forall a. P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    pure $ foldl (\a s -> s a) x ss

infixr 6 startSuff as |>>

nonArrayType :: P Type
nonArrayType = PrimType <$> primType <|> RefType <$> ClassRefType <$> classType

arrayCreation :: P Exp
arrayCreation = do
    tok KW_New
    t <- nonArrayType
    f <- (PC.try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             pure $ \t -> ArrayCreateInit t (List.length ds) ai) <|>
         (do 
             des <- list1 $ PC.try $ brackets expression
             ds  <- list  $ brackets empty
             pure $ \t -> ArrayCreate t des (List.length ds))
    pure $ f t

infixExp :: P Exp 
infixExp = fix $ \_ -> unaryExp 

unaryExp :: P Exp 
unaryExp = fix $ \_ -> postfixExp 

postfixExp :: P Exp 
postfixExp = fix $ \_ -> postfixExpNES

postfixExpNES :: P Exp 
postfixExpNES = fix $ \_ -> primary

fieldAccessNPS :: P FieldAccess
fieldAccessNPS =
    (do tok KW_Super *> period
        i <- ident
        pure $ SuperFieldAccess i) <|>
    (do n <- name
        period *> tok KW_Super *> period
        i <- ident
        pure $ ClassFieldAccess n i)

fieldAccessSuffix :: P (Exp -> FieldAccess)
fieldAccessSuffix = do
    period
    i <- ident
    pure $ \p -> PrimaryFieldAccess p i

fieldAccess :: P FieldAccess
fieldAccess = 
    PC.try fieldAccessNPS <|> 
    (do
        p  <- primary
        ss <- list primarySuffix
        let fap = foldl (\a s -> s a) p ss
        case fap of
            FieldAccess fa -> pure fa
            _ -> fail "")

instanceCreationSuffix :: P (Exp -> Exp)
instanceCreationSuffix = do 
    period *> tok KW_New
    tas <- lopt typeArgs
    i   <- ident
    as  <- args
    mcb <- optMaybe classBody
    pure $ \p -> QualInstanceCreation p tas i as mcb

stmt :: P Stmt
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens expression
        (PC.try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               pure $ IfThenElse e th el) <|>
           (do th <- stmt
               pure $ IfThen e th)
    whileStmt = do
        tok KW_While
        e   <- parens expression
        s   <- stmt
        pure $ While e s
    forStmt = do
        tok KW_For
        f <- parens $
            (PC.try $ do
                fi <- optMaybe forInit
                semiColon
                e  <- optMaybe expression
                semiColon
                fu <- optMaybe forUp
                pure $ BasicFor fi e fu) <|>
            (do 
                ms <- list modifier
                t  <- type_
                i  <- ident
                colon
                e  <- expression
                pure $ EnhancedFor ms t i e)
        s <- stmt
        pure $ f s
    labeledStmt = PC.try $ do
        lbl <- ident
        colon
        s   <- stmt
        pure $ Labeled lbl s

stmtNSI :: P Stmt
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens expression
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        pure $ IfThenElse e th el
    whileStmt = do
        tok KW_While
        e <- parens expression
        s <- stmtNSI
        pure $ While e s
    forStmt = do
        tok KW_For
        f <- parens $ (PC.try $ do
            fi <- optMaybe forInit
            semiColon
            e  <- optMaybe expression
            semiColon
            fu <- optMaybe forUp
            pure $ BasicFor fi e fu)
            <|> (do
            ms <- list modifier
            t  <- type_
            i  <- ident
            colon
            e  <- expression
            pure $ EnhancedFor ms t i e)
        s <- stmtNSI
        pure $ f s
    labeledStmt = PC.try $ do
        i <- ident
        colon
        s <- stmtNSI
        pure $ Labeled i s

stmtNoTrail :: P Stmt
stmtNoTrail =
    -- empty statement
    const Empty <$> semiColon <|>
    -- inner block
    StmtBlock <$> block <|>
    -- assertions
    (endSemi $ do
        tok KW_Assert
        e   <- expression
        me2 <- optMaybe $ colon *> expression
        pure $ Assert e me2) <|>
    -- switch stmts

    -- (do tok KW_Switch
    --     e  <- parens expression
    --     sb <- switchBlock
    --     pure $ Switch e sb) <|>

    -- do-while loops
    (endSemi $ do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens expression
        pure $ Do s e) <|>
    -- break
    (endSemi $ do
        tok KW_Break
        mi <- optMaybe ident
        pure $ Break mi) <|>
    -- continue
    (endSemi $ do
        tok KW_Continue
        mi <- optMaybe ident
        pure $ Continue mi) <|>
    -- pure
    (endSemi $ do
        tok KW_Return
        me <- optMaybe expression
        pure $ Return me) <|>
    -- throw
    (endSemi $ do
        tok KW_Throw
        e <- expression
        pure $ Throw e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        b <- block
        c <- list catch
        mf <- optMaybe $ tok KW_Finally *> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        pure $ Try b c mf) <|>
    -- expressions as stmts
    ExpStmt <$> endSemi stmtExp

-- For loops

forInit :: P ForInit
forInit = 
    (PC.try 
        (do 
            (Tuple3 m t vds) <- localVarDecl
            pure $ ForLocalVars m t vds)) <|>
    (seplist1 stmtExp comma >>= (pure <<< ForInitExps))

forUp :: P (List Exp)
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P (List SwitchBlock)
switchBlock = braces $ list switchStmt

switchStmt :: P SwitchBlock
switchStmt = do
    lbl <- switchLabel
    bss <- list blockStmt
    pure $ SwitchBlock lbl bss

switchLabel :: P SwitchLabel
switchLabel = (tok KW_Default *> colon *> pure Default) <|>
    (do tok KW_Case
        e <- expression
        colon
        pure $ SwitchCase e)

catch :: P Catch
catch = do
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    pure $ Catch fp b

----------- Arrays -------

arrayAccessNPS :: P ArrayIndex
arrayAccessNPS = do
    n <- name
    e <- list1 $ brackets (fix $ \_ -> expression)
    pure $ ArrayIndex (ExpName n) e

arrayAccessSuffix :: P (Exp -> ArrayIndex)
arrayAccessSuffix = do
    e <- list1 $ brackets (fix $ \_ -> expression)
    pure $ \ref -> ArrayIndex ref e

arrayAccess = 
    PC.try arrayAccessNPS <|> 
    (do
        p <- primaryNoNewArrayNPS
        ss <- list primarySuffix
        let aap = foldl (\a s -> s a) p ss
        case aap of
            ArrayAccess ain -> pure ain
            _ -> fail "")

methodRef :: P Exp
methodRef = 
    MethodRef <$> (name <* period) <*> ident
----------------------------------------------------------------------------
-- Statements

block :: P Block
block = braces $ Block <$> (list $ fix $ \_ -> blockStmt)

blockStmt :: P BlockStmt
blockStmt = 
    (PC.try $ do
        ms  <- list $ fix $ \_ -> modifier
        cd  <- fix $ \_ -> classDecl
        pure $ LocalClass (cd ms)) <|>
    (PC.try $ do  
        (Tuple3 m t vds) <- endSemi $ (fix $ \_ -> localVarDecl) 
        pure $ LocalVars m t vds) 



----------------- Type parameters and arguments -----------------
typeParams :: P (List TypeParam)
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = do
    i  <- ident
    rf <- lopt extends
    pure $ TypeParam i rf

typeArgs :: P (List TypeArgument)
typeArgs = fix \_ -> do
    angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg = fix \_ -> do 
    r <- refType
    pure $ ActualType r

---------------- Types ---------------------
    
extends :: P (List RefType)
extends = tok KW_Extends *> refTypeList

implements :: P (List RefType)
implements = tok KW_Implements *> refTypeList

refTypeArgs :: P (List RefType)
refTypeArgs = angles refTypeList

refTypeList :: P (List RefType)
refTypeList = seplist1 refType comma

refType :: P RefType
refType = fix \_ -> 
    (do pt <- primType
        l <- list1 arrBrackets
        let bs = fromMaybe mempty $ List.tail l 
        pure $ foldl (\f _ -> ArrayType <<< RefType <<< f)
                        (ArrayType <<< PrimType) bs pt) <|>
    (do ct <- classType
        bs <- list arrBrackets
        pure $ foldl (\f _ -> ArrayType <<< RefType <<< f)
                            ClassRefType bs ct) <?> "refType"

classType :: P ClassType
classType = fix \_ -> do 
    c <- seplist1 classTypeSpec period 
    pure $ ClassType c

classTypeSpec :: P (Tuple Ident (List TypeArgument))
classTypeSpec = do
    i   <- ident
    tas <- lopt typeArgs
    pure $ Tuple i tas

type_ :: P Type
type_ = PC.try (RefType <$> refType) <|> PrimType <$> primType

resultType :: P (Maybe Type)
resultType = tok KW_Void *> pure Nothing <|> Just <$> type_ <?> "resultType"

primType :: P PrimType
primType = 
    tok KW_Boolean  *> pure BooleanT  <|>
    tok KW_Object   *> pure ObjectT   <|>
    tok KW_Decimal  *> pure DecimalT  <|>
    tok KW_Integer  *> pure IntegerT  <|>
    tok KW_Long     *> pure LongT     <|>
    tok KW_Blob     *> pure BlobT     <|>
    tok KW_Date     *> pure DateT     <|>
    tok KW_Datetime *> pure DatetimeT <|>
    tok KW_Time     *> pure TimeT     <|>
    tok KW_Double   *> pure DoubleT

----------------- Operators ----------------

preIncDecOp :: P (Exp -> Exp)
preIncDecOp =
    tok Op_PPlus  *> pure PreIncrement <|>
    tok Op_MMinus *> pure PreDecrement

prefixOp :: P (Exp -> Exp)
prefixOp =
    tok Op_Bang  *> pure PreNot       <|>
    tok Op_Tilde *> pure PreBitCompl  <|>
    tok Op_Plus  *> pure PrePlus      <|>
    tok Op_Minus *> pure PreMinus    

postfixOp :: P (Exp -> Exp)
postfixOp =
    tok Op_PPlus  *> pure PostIncrement <|>
    tok Op_MMinus *> pure PostDecrement

assignOp :: P AssignOp
assignOp =
    tok Op_Equal    *> pure EqualA   <|>
    tok Op_StarE    *> pure MultA    <|>
    tok Op_SlashE   *> pure DivA     <|>
    tok Op_PercentE *> pure RemA     <|>
    tok Op_PlusE    *> pure AddA     <|>
    tok Op_MinusE   *> pure SubA     <|>
    tok Op_LShiftE  *> pure LShiftA  <|>
    tok Op_RShiftE  *> pure RShiftA  <|>
    tok Op_RRShiftE *> pure RRShiftA <|>
    tok Op_AndE     *> pure AndA     <|>
    tok Op_CaretE   *> pure XorA     <|>
    tok Op_OrE      *> pure OrA     

infixCombineOp :: P Op
infixCombineOp = 
    tok Op_And   *> pure And  <|>
    tok Op_Caret *> pure Xor  <|>
    tok Op_Or    *> pure Or   <|>
    tok Op_AAnd  *> pure CAnd <|>
    tok Op_OOr   *> pure COr 


infixOp :: P Op
infixOp =
    tok Op_Star    *> pure Mult   <|>
    tok Op_Slash   *> pure Div    <|>
    tok Op_Percent *> pure Rem    <|>
    tok Op_Plus    *> pure Add    <|>
    tok Op_Minus   *> pure Sub    <|>
    tok Op_LShift  *> pure LShift <|>
    tok Op_LThan   *> pure LThan  <|>

    (PC.try $ do
       tok Op_GThan   
       tok Op_GThan   
       tok Op_GThan
       pure RRShift   ) <|>
           
    (PC.try $ do
       tok Op_GThan 
       tok Op_GThan
       pure RShift    ) <|>
           
    tok Op_GThan  *> pure GThan  <|>                                          
    tok Op_LThanE *> pure LThanE <|>
    tok Op_GThanE *> pure GThanE <|>
    tok Op_Equals *> pure Equal  <|>
    tok Op_BangE  *> pure NotEq 

----------------- Utils --------------------

seplist :: forall a sep. P a -> P sep -> P (List a)
seplist p sep = PC.option mempty $ seplist1 p sep

seplist1 :: forall a sep. P a -> P sep -> P (List a)
seplist1 p sep = do 
    a <- p
    PC.try (loopList a) <|> (pure $ List.singleton a)
    where 
        loopList a' = do 
            _ <- sep
            as <- seplist1 p sep
            pure (a':as)

list ::  forall a. P a -> P (List a)
list = PC.option mempty <<< list1

list1 :: forall a. P a -> P (List a)
list1 = List.someRec

endSemi :: forall a. P a -> P a
endSemi p = p >>= \a -> semiColon *> pure a

arrBrackets :: P Unit
arrBrackets = brackets $ pure unit

parens   = PC.between (tok OpenParen)  (tok CloseParen)
braces   = PC.between (tok OpenCurly)  (tok CloseCurly)
brackets = PC.between (tok OpenSquare) (tok CloseSquare)
angles   = PC.between (tok Op_LThan)   (tok Op_GThan)

comma = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

--token :: forall t s a. (Position -> t -> List t -> Position) -> (t -> String) -> (t -> Maybe a) -> P a
token nextpos showt test = do
    input <- gets \(ParseState input _ _) -> input
    case List.uncons input of
      Nothing -> fail "Unexpected EOF"
      Just {head,tail} -> do
        case test head of 
            Nothing -> fail $ "Unexpected token: " <> showt head
            Just  x -> do
                _ <- modify_ \(ParseState _ pos _) -> ParseState tail (nextpos pos head tail) true
                
                pure x

javaToken' :: forall a. (Token -> Maybe a) -> P a
javaToken' test =  token posT showT testT
    where 
        showT (L _ t) = show t
        posT  _ (L p _) _ = Newtype.unwrap p
        testT (L _ t) = test t

javaToken :: forall a. (Token -> Maybe a) -> P a
javaToken f = do
    (L _ t) <- PT.token (\(L pos _) -> Newtype.unwrap pos)
    p <- position
    maybe (fail $ "error parsing toking: " <> show t <> ": " <> show p) pure $ f t

tok :: Token -> P Unit 
tok t = javaToken' (\r -> if r == t then Just unit else Nothing)

-- tok :: Token -> P Unit
-- tok t = javaToken (\r -> if r == t then Just unit else Nothing)

optMaybe :: forall a. P a -> P (Maybe a)
optMaybe = PC.optionMaybe 

bopt :: forall a. P a -> P Boolean
bopt p = optMaybe p >>= \ma -> pure $ isJust ma 

lopt :: forall a. P (List a) -> P (List a)
lopt p = do mas <- optMaybe p
            case mas of
             Nothing -> pure mempty
             Just as -> pure as

empty :: P Unit
empty = pure unit