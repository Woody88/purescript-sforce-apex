module Language.Apex.Parser where 



import Language.Apex.Syntax
import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.State (gets, modify_)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.Newtype as Newtype
import Data.Tuple (Tuple(..))
import Language.Apex.Lexer (lexApex)
import Language.Types (Token(..), L(..), KeywordT(..), OperatorT(..), LiteralT(..), DMLOpT(..), SharingT(..), IdentT(..), SymbolT(..))
import Language.Internal (langToken, tok', optMaybe, empty, bopt, lopt, tok, seplist, seplist1, list, list1)
import Language.Apex.Syntax.Types (DML(..), ClassType(..), Ident(..), Literal(..), Name(..), PrimType(..), RefType(..), Type(..), TypeArgument(..), TypeParam(..))
import Text.Parsing.Parser (ParseState(..), Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Token as PT

type P = Parser (List (L Token))

------------- Top Level parsing -----------------

-- parseCompilationUnit :: String -> Either ParseError CompilationUnit
-- parseCompilationUnit input = runParser (lexApex input) compilationUnit

------------- Compilation Unit -----------------
compilationUnit :: P CompilationUnit
compilationUnit = do 
    tds <- list1 typeDecl
    pure $ CompilationUnit (List.catMaybes tds)

literal :: P Literal
literal = langToken $ \t -> case t of
    LiteralTok (IntegerTok i) -> Just (Integer i)
    LiteralTok (LongTok    l) -> Just (Long l)
    LiteralTok (DoubleTok  d) -> Just (Double d)
    LiteralTok (StringTok  s) -> Just (String s)
    LiteralTok (BoolTok    b) -> Just (Boolean b)
    LiteralTok (NullTok)      -> Just Null 
    _ -> Nothing

dml :: P DML 
dml = langToken $ \t -> case t of
    DMLOpTok (KW_Update)   -> Just $ Update
    DMLOpTok (KW_Insert)   -> Just $ Insert
    DMLOpTok (KW_Upsert)   -> Just $ Upsert
    DMLOpTok (KW_Delete)   -> Just $ Delete
    DMLOpTok (KW_Undelete) -> Just $ Undelete
    DMLOpTok (KW_Merge)    -> Just $ Merge   
    _ -> Nothing

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = langToken $ \t -> case t of
    IdentTok (IdentT s) -> Just $ Ident s
    _ -> Nothing

fieldDecl :: P (Mod MemberDecl)
fieldDecl = endOptSemi $ do
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
    (const Nothing <$> semiColon <|> Just <$> (fix $ \_ -> block)) <?> "unexpected method body"

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
    mdl <- list (fix $ \_ -> modifier)
    de <- (do cd <- classDecl
              pure $ \ms -> ClassTypeDecl (cd ms)) <|>
          (do id <- interfaceDecl
              pure $ \ms -> InterfaceTypeDecl (id ms))
    pure $ de mdl

classDecl :: P (Mod ClassDecl)
classDecl = fix $ \_ -> normalClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod ClassDecl)
normalClassDecl = do 
    tok (KeywordTok KW_Class) 
    i    <- ident 
    tps  <- lopt typeParams
    ext  <- optMaybe extends 
    imp  <- lopt implements
    body <- classBody
    pure $ \ms -> ClassDecl ms i tps (ext >>= List.head) imp body

enumClassDecl :: P (Mod ClassDecl)
enumClassDecl = do
    tok (KeywordTok KW_Enum)
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
       mst <- bopt (tok (KeywordTok KW_Static))
       blk <- block
       pure $ Just $ InitDecl mst blk) <|>
    (PC.try $ do 
        ms  <- list (fix $ \_ -> modifier)
        dec <- memberDecl
        pure $ Just $ MemberDecl (dec ms))
    <?> "unexpected class body statement"

memberDecl :: P (Mod MemberDecl)
memberDecl = fix $ \_ -> 
    (PC.try $ do
        cd  <- classDecl
        pure $ \ms -> MemberClassDecl (cd ms)) <|>
    (PC.try $ do
        id  <- interfaceDecl
        pure $ \ms -> MemberInterfaceDecl (id ms)) <|>
    PC.try methodDecl <|>
    PC.try fieldDecl <|>
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
        tok (KeywordTok KW_This)
        as  <- args
        pure $ ThisInvoke tas as) <|>
    (PC.try $ do
        tas <- lopt refTypeArgs
        tok (KeywordTok KW_Super)
        as  <- args
        pure $ SuperInvoke tas as) <|>
    (do pri <- fix $ \_ -> primary
        period
        tas <- lopt refTypeArgs
        tok (KeywordTok KW_Super)
        as  <- args
        pure $ PrimarySuperInvoke pri tas as)

args :: P (List Argument)
args = parens $ seplist (fix $ \_ -> expression) comma

-- Interface Declaration 

interfaceDecl :: P (Mod InterfaceDecl)
interfaceDecl = do
    tok (KeywordTok KW_Interface)
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
interfaceMemberDecl = fix $ \_ -> absMethodDecl 

-- MethodDecl without any implementation. I.e: interface method definition
absMethodDecl :: P (Mod MemberDecl)
absMethodDecl = do
    tps <- lopt typeParams
    rt  <- resultType
    id  <- ident
    fps <- formalParams
    semiColon
    pure $ \ms -> MethodDecl ms tps rt id fps Nothing (MethodBody Nothing)

-- Modifiers

modifier :: P Modifier
modifier =
        tok (KeywordTok KW_Static)        *> pure Static
    <|> tok (KeywordTok KW_Public)        *> pure Public
    <|> tok (KeywordTok KW_Protected)     *> pure Protected
    <|> tok (KeywordTok KW_Private)       *> pure Private
    <|> tok (KeywordTok KW_Abstract)      *> pure Abstract
    <|> tok (KeywordTok KW_Global)        *> pure Global
    <|> tok (KeywordTok KW_Virtual)       *> pure Virtual
    <|> tok (KeywordTok KW_Final)         *> pure Final
    <|> tok (KeywordTok KW_Transient)     *> pure Transient
    <|> tok (SharingTok KW_With_Share)    *> pure With_Share
    <|> tok (SharingTok KW_Without_Share) *> pure Without_Share
    <|> tok (SharingTok KW_Inherit_Share) *> pure Inherit_Share
    <|> tok (KeywordTok KW_Override)      *> pure Override 
    <|> Annotation <$> (fix $ \_ -> annotation)

annotation :: P Annotation
annotation = do 
    annName <- tok (OperatorTok Op_AtSign) *> name
    PC.try (parens evlist >>= \annKV -> pure $ NormalAnnotation { annName,  annKV }) <|> (pure $ MarkerAnnotation { annName })


evlist :: P (List (Tuple Ident ElementValue))
evlist = fix $ \_ -> list elementValuePair 

elementValuePair :: P (Tuple Ident ElementValue)
elementValuePair = fix $ \_ -> Tuple <$> ident <* tok (OperatorTok Op_Equal) <*> elementValue

elementValue :: P ElementValue
elementValue = fix $ \_ -> EVVal <$> InitExp <$> infixExp

------------ Variable declarations ----------------

localVarDecl :: P (Tuple3 (List Modifier) Type (List VarDecl))
localVarDecl = do
    ms <- list (fix $ \_ -> modifier)
    typ <- type_
    vds <- varDecls
    pure $ Tuple3 ms typ vds

accessor :: P Accessor
accessor = fix $ \_ -> do
    md  <- optMaybe modifier 
    acv <- accessorVar
    _   <- PC.optional semiColon
    vi  <- optMaybe $ block
    pure $ Accessor md acv vi

accessorVar :: P AccessorVar
accessorVar = (tok (IdentTok $ IdentT "get") *> pure Getter) <|> tok (IdentTok $ IdentT "set") *> pure Setter <?> "Unpexpected token at accessor position"

varDecls :: P (List VarDecl)
varDecls = fix $ \_ -> seplist1 varDecl comma 

varDecl :: P VarDecl 
varDecl = do
    vdi <- varDeclId
    vi  <- optMaybe $ tok (OperatorTok Op_Equal) *> varInit 
    pure $ VarDecl vdi vi 
    
varDeclId :: P VarDeclId 
varDeclId = do 
    id <- ident
    abs <- list arrBrackets   -- should probably remove this, there are now way to create array matrix in apex. I.e: String[][] 
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
expression = fix $ \_ -> assignExp 

assignExp :: P Exp
assignExp = fix $ \_ -> PC.try methodRef <|> PC.try assignment <|> condExp

stmtExp :: P Exp
stmtExp = fix $ \_ -> 
    PC.try dmlExp 
    <|> PC.try preIncDec
    <|> PC.try postIncDec
    <|> PC.try assignment
    <|> PC.try methodInvocationExp
    <|> PC.try methodRef
    <|> instanceCreation
    <?> "unexpected statement expression"

dmlExp :: P Exp 
dmlExp = do 
    d <- dml
    n <- name
    pure $ DML d (ExpName n)

preIncDec :: P Exp
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    pure $ op e

postIncDec :: P Exp
postIncDec = fix $ \_ -> do
    e <- postfixExpNES
    ops <- list1 postfixOp
    pure $ foldl (\a s -> s a) e ops

assignment :: P Exp
assignment = fix $ \_ ->  do
    lh <- lhs
    op <- assignOp
    e  <- expression
    pure $ Assign lh op e

lhs :: P Lhs
lhs = fix $ \_ -> 
    PC.try (FieldLhs <$> fieldAccess)
    <|> PC.try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name
    <?> "unexpected left hand side"

methodInvocationSuffix :: P (Exp -> MethodInvocation)
methodInvocationSuffix = do
    period
    rts <- lopt refTypeArgs
    i   <- ident
    as  <- args
    pure $ \p -> PrimaryMethodCall p mempty i as

methodInvocationNPS :: P MethodInvocation
methodInvocationNPS =
    (do tok (KeywordTok KW_Super) *> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        pure $ SuperMethodCall rts i as) <|>
    (do nm <- name
        f <- (do 
                as <- args
                pure $ \n -> MethodCall n as) <|>
             (period *> do
                msp <- optMaybe (tok (KeywordTok KW_Super) *> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                pure $ \n -> mc n rts i as)
        pure $ f nm)

methodInvocationExp :: P Exp
methodInvocationExp = fix $ \_ -> 
    PC.try 
    (do
        p <- primaryNPS
        ss <- list primarySuffix
        let mip = foldl (\a s -> s a) p ss
        case mip of
            MethodInv _ -> pure mip
            _ -> fail "") <|>
     (MethodInv <$> methodInvocationNPS) <?> "unexpected method invocation expression"

instanceCreationNPS :: P Exp
instanceCreationNPS = do 
    tok (KeywordTok KW_New)
    tas <- lopt typeArgs
    ct  <- classType
    as  <- args
    mcb <- optMaybe classBody
    pure $ InstanceCreation tas ct as mcb

instanceCreation :: P Exp
instanceCreation = fix $ \_ -> 
    PC.try instanceCreationNPS <|> 
    (do
        p <- primaryNPS
        ss <- list primarySuffix
        let icp = foldl (\a s -> s a) p ss
        case icp of
            (QualInstanceCreation _ _ _ _ _) -> pure icp
            _ -> fail "unexpected instance creation case")

primary :: P Exp 
primary = fix $ \_ -> primaryNPS |>> primarySuffix

primaryNPS :: P Exp
primaryNPS = fix $ \_ -> PC.try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArrayNPS :: P Exp
primaryNoNewArrayNPS = fix $ \_ -> 
    Lit <$> literal <|>
    const This <$> tok (KeywordTok KW_This) <|>
    parens (fix $ \_ -> expression) <|>
    -- TODO: These two following should probably be merged more
    (PC.try $ do
        rt <- resultType
        period *> tok (KeywordTok KW_Class)
        pure $ ClassLit rt) <|>
    (PC.try $ do
        n <- name
        period *> tok (KeywordTok KW_This)
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
    tok (KeywordTok KW_New)
    tp <- nonArrayType
    f <- (PC.try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             pure $ \t -> ArrayCreateInit t (List.length ds) ai) <|>
         (do 
             des <- list1 $ PC.try $ brackets expression
             ds  <- list  $ brackets empty
             pure $ \t -> ArrayCreate t des (List.length ds))
    pure $ f tp

condExp :: P Exp
condExp = fix $ \_ -> do
    ie <- infixExp
    ces <- list condExpSuffix
    pure $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (Exp -> Exp)
condExpSuffix = fix $ \_ ->  do
    tok (OperatorTok Op_Query)
    th <- expression
    _ <- colon
    el <- condExp
    pure $ \ce -> Cond ce th el

infixExp :: P Exp 
infixExp = fix $ \_ -> do 
    ue <- unaryExp 
    ies <- list infixExpSuffix
    pure $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (Exp -> Exp)
infixExpSuffix = 
    (do
      op <- infixCombineOp
      ie2 <- infixExp
      pure $ \ie1 -> BinOp ie1 op ie2) <|>
    (do op <- infixOp
        e2 <- unaryExp
        pure $ \e1 -> BinOp e1 op e2) <|>
    (do tok (KeywordTok KW_Instanceof)
        t  <- refType
        pure $ \e1 -> InstanceOf e1 t)

unaryExp :: P Exp 
unaryExp = fix $ \_ -> 
    PC.try preIncDec <|>
    PC.try (do
        op <- prefixOp
        ue <- unaryExp
        pure $ op ue) <|>
    PC.try (do
        t <- parens type_
        e <- unaryExp
        pure $ Cast t e) <|>
    postfixExp

postfixExp :: P Exp 
postfixExp = fix $ \_ -> do
    pe <- postfixExpNES
    ops <- list postfixOp
    pure $ foldl (\a s -> s a) pe ops


postfixExpNES :: P Exp 
postfixExpNES = fix $ \_ -> PC.try primary <|> ExpName <$> name

fieldAccessNPS :: P FieldAccess
fieldAccessNPS =
    (do tok (KeywordTok KW_Super) *> period
        i <- ident
        pure $ SuperFieldAccess i) <|>
    (do n <- name
        period *> tok (KeywordTok KW_Super) *> period
        i <- ident
        pure $ ClassFieldAccess n i)

fieldAccessSuffix :: P (Exp -> FieldAccess)
fieldAccessSuffix = do
    period
    i <- ident
    pure $ \p -> PrimaryFieldAccess p i

fieldAccess :: P FieldAccess
fieldAccess = fix $ \_ -> 
    PC.try fieldAccessNPS <|> 
    (do
        p  <- primary
        ss <- list primarySuffix
        let fap = foldl (\a s -> s a) p ss
        case fap of
            FieldAccess fa -> pure fa
            _ -> fail "unpexcted field access case")

instanceCreationSuffix :: P (Exp -> Exp)
instanceCreationSuffix = do 
    period *> tok (KeywordTok KW_New)
    tas <- lopt typeArgs
    i   <- ident
    as  <- args
    mcb <- optMaybe classBody
    pure $ \p -> QualInstanceCreation p tas i as mcb

stmt :: P Stmt
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok (KeywordTok KW_If)
        e   <- parens expression
        (PC.try $
            do th <- stmtNSI
               tok (KeywordTok KW_Else)
               el <- stmt
               pure $ IfThenElse e th el) <|>
           (do th <- stmt
               pure $ IfThen e th)
    whileStmt = do
        tok (KeywordTok KW_While)
        e   <- parens expression
        s   <- stmt
        pure $ While e s
    forStmt = do
        tok (KeywordTok KW_For)
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
        tok (KeywordTok KW_If)
        e  <- parens expression
        th <- stmtNSI
        tok (KeywordTok KW_Else)
        el <- stmtNSI
        pure $ IfThenElse e th el
    whileStmt = do
        tok (KeywordTok KW_While)
        e <- parens expression
        s <- stmtNSI
        pure $ While e s
    forStmt = do
        tok (KeywordTok KW_For)
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
stmtNoTrail = fix $ \_ -> 
    -- empty statement
    const Empty <$> semiColon <|>
    -- inner block
    StmtBlock <$> block <|>
    -- assertions
    (endSemi $ do
        tok (KeywordTok KW_Assert)
        e   <- expression
        me2 <- optMaybe $ colon *> expression
        pure $ Assert e me2) <|>
    -- switch stmts

    (do tok (KeywordTok KW_Switch)
        e  <- PC.try (parens expression) <|> expression
        sb <- switchBlock
        pure $ Switch e sb) <|>

    -- do-while loops
    (endSemi $ do
        tok (KeywordTok KW_Do)
        s <- stmt
        tok (KeywordTok KW_While)
        e <- parens expression
        pure $ Do s e) <|>
    -- break
    (endSemi $ do
        tok (KeywordTok KW_Break)
        mi <- optMaybe ident
        pure $ Break mi) <|>
    -- continue
    (endSemi $ do
        tok (KeywordTok KW_Continue)
        mi <- optMaybe ident
        pure $ Continue mi) <|>
    -- pure
    (endSemi $ do
        tok (KeywordTok KW_Return)
        me <- optMaybe expression
        pure $ Return me) <|>
    -- throw
    (endSemi $ do
        tok (KeywordTok KW_Throw)
        e <- expression
        pure $ Throw e) <|>
    -- try-catch, both with and without a finally clause
    (do tok (KeywordTok KW_Try)
        b <- block
        c <- list catch
        mf <- optMaybe $ tok (KeywordTok KW_Finally) *> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        pure $ Try b c mf) <|>
    -- expressions as stmts
    ExpStmt <$> endSemi stmtExp

-- For loops

forInit :: P ForInit
forInit = fix $ \_ -> 
    (PC.try 
        (do 
            (Tuple3 m t vds) <- localVarDecl
            pure $ ForLocalVars m t vds)) <|>
    (seplist1 stmtExp comma >>= (pure <<< ForInitExps))

forUp :: P (List Exp)
forUp = fix $ \_ -> seplist1 stmtExp comma

-- Switches

switchBlock :: P (List SwitchBlock)
switchBlock = fix $ \_ -> braces $ list switchStmt

switchStmt :: P SwitchBlock
switchStmt = fix $ \_ -> do
    lbl <- switchLabel
    bl <- block 
    pure $ SwitchBlock lbl bl

switchLabel :: P SwitchLabel
switchLabel = fix $ \_ -> 
    (tok (KeywordTok KW_WhenElse) *> pure WhenElse)  <|>
    (do tok (KeywordTok KW_When)
        e <- expression
        pure $ SwitchCase e)

catch :: P Catch
catch = do
    tok (KeywordTok KW_Catch)
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

arrayAccess :: P ArrayIndex
arrayAccess = fix $ \_ -> 
    PC.try arrayAccessNPS <|> 
    (do
        p <- primaryNoNewArrayNPS
        ss <- list primarySuffix
        let aap = foldl (\a s -> s a) p ss
        case aap of
            ArrayAccess ain -> pure ain
            _ -> fail "unexpected array accesss case")

methodRef :: P Exp
methodRef = 
    MethodRef <$> (name <* period) <*> ident
----------------------------------------------------------------------------
-- Statements

block :: P Block
block = fix $ \_ -> braces $ Block <$> list blockStmt

blockStmt :: P BlockStmt
blockStmt = fix $ \_ -> 
    (PC.try $ do
        ms  <- list $ modifier
        cd  <- classDecl
        pure $ LocalClass (cd ms)) <|>
    (PC.try $ do  
        (Tuple3 m t vds) <- endSemi $ localVarDecl
        pure $ LocalVars m t vds) <|>
    (PC.try $ do 
        lacs <- list1 accessor
        pure $ Property lacs) <|>
    (BlockStmt <$> stmt) <?> "unexpected blobck stmt"

   

----------------- Type parameters and arguments -----------------
typeParams :: P (List TypeParam)
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = 
    (do
        i  <- ident
        rf <- lopt extends
        pure $ TypeParam i rf)

typeArgs :: P (List TypeArgument)
typeArgs = fix \_ -> do
    angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg = fix \_ -> do 
    r <- refType
    pure $ ActualType r

---------------- Types ---------------------
    
extends :: P (List RefType)
extends = tok (KeywordTok KW_Extends) *> refTypeList

implements :: P (List RefType)
implements = tok (KeywordTok KW_Implements) *> refTypeList

refTypeArgs :: P (List RefType)
refTypeArgs = angles refTypeList

refTypeList :: P (List RefType)
refTypeList = seplist1 refType comma

refType :: P RefType
refType = fix \_ -> 
    PC.try (do 
        pt <- primType
        l <- list1 arrBrackets
        let bs = fromMaybe mempty $ List.tail l 
        pure $ foldl (\f _ -> ArrayType <<< RefType <<< f) (ArrayType <<< PrimType) bs pt) <|>
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
    i   <- PC.try primToIdent <|> ident
    tas <- lopt typeArgs
    pure $ Tuple i tas
    where 
        primToIdent = (Ident <<< show) <$> primType 

type_ :: P Type
type_ = PC.try (RefType <$> refType) <|> PrimType <$> primType

resultType :: P (Maybe Type)
resultType = tok (KeywordTok KW_Void) *> pure Nothing <|> Just <$> type_ <?> "resultType"

primType :: P PrimType
primType = 
    tok (KeywordTok KW_Boolean)  *> pure BooleanT  <|>
    tok (KeywordTok KW_Object)   *> pure ObjectT   <|>
    tok (KeywordTok KW_Decimal)  *> pure DecimalT  <|>
    tok' "id"       *> pure IdT       <|>
    tok (KeywordTok KW_String)   *> pure StringT       <|>
    tok (KeywordTok KW_Integer)  *> pure IntegerT  <|>
    tok (KeywordTok KW_Long)     *> pure LongT     <|>
    tok (KeywordTok KW_Blob)     *> pure BlobT     <|>
    tok (KeywordTok KW_Date)     *> pure DateT     <|>
    tok (KeywordTok KW_Datetime) *> pure DatetimeT <|>
    tok (KeywordTok KW_Time)     *> pure TimeT     <|>
    tok (KeywordTok KW_Double)   *> pure DoubleT

----------------- Operators ----------------

preIncDecOp :: P (Exp -> Exp)
preIncDecOp =
    tok (OperatorTok Op_PPlus)  *> pure PreIncrement <|>
    tok (OperatorTok Op_MMinus) *> pure PreDecrement

prefixOp :: P (Exp -> Exp)
prefixOp =
    tok (OperatorTok Op_Bang)  *> pure PreNot       <|>
    tok (OperatorTok Op_Tilde) *> pure PreBitCompl  <|>
    tok (OperatorTok Op_Plus)  *> pure PrePlus      <|>
    tok (OperatorTok Op_Minus) *> pure PreMinus    

postfixOp :: P (Exp -> Exp)
postfixOp =
    tok (OperatorTok Op_PPlus)  *> pure PostIncrement <|>
    tok (OperatorTok Op_MMinus) *> pure PostDecrement

assignOp :: P AssignOp
assignOp =
    tok (OperatorTok Op_Equal)    *> pure EqualA   <|>
    tok (OperatorTok Op_StarE)    *> pure MultA    <|>
    tok (OperatorTok Op_SlashE)   *> pure DivA     <|>
    tok (OperatorTok Op_PercentE) *> pure RemA     <|>
    tok (OperatorTok Op_PlusE)    *> pure AddA     <|>
    tok (OperatorTok Op_MinusE)   *> pure SubA     <|>
    tok (OperatorTok Op_LShiftE)  *> pure LShiftA  <|>
    tok (OperatorTok Op_RShiftE)  *> pure RShiftA  <|>
    tok (OperatorTok Op_RRShiftE) *> pure RRShiftA <|>
    tok (OperatorTok Op_AndE)     *> pure AndA     <|>
    tok (OperatorTok Op_CaretE)   *> pure XorA     <|>
    tok (OperatorTok Op_OrE)      *> pure OrA     

infixCombineOp :: P Op
infixCombineOp = 
    tok (OperatorTok Op_And)   *> pure And  <|>
    tok (OperatorTok Op_Caret) *> pure Xor  <|>
    tok (OperatorTok Op_Or)    *> pure Or   <|>
    tok (OperatorTok Op_AAnd)  *> pure CAnd <|>
    tok (OperatorTok Op_OOr)   *> pure COr 


infixOp :: P Op
infixOp =
    tok (OperatorTok Op_Star)    *> pure Mult   <|>
    tok (OperatorTok Op_Slash)   *> pure Div    <|>
    tok (OperatorTok Op_Percent) *> pure Rem    <|>
    tok (OperatorTok Op_Plus)    *> pure Add    <|>
    tok (OperatorTok Op_Minus)   *> pure Sub    <|>
    tok (OperatorTok Op_LShift)  *> pure LShift <|>
    tok (OperatorTok Op_LThan)   *> pure LThan  <|>

    (PC.try $ do
       tok (OperatorTok Op_GThan)   
       tok (OperatorTok Op_GThan)   
       tok (OperatorTok Op_GThan)
       pure RRShift   ) <|>
           
    (PC.try $ do
       tok (OperatorTok Op_GThan) 
       tok (OperatorTok Op_GThan)
       pure RShift    ) <|>
           
    tok (OperatorTok Op_GThan)  *> pure GThan  <|>                                          
    tok (OperatorTok Op_LThanE) *> pure LThanE <|>
    tok (OperatorTok Op_GThanE) *> pure GThanE <|>
    tok (OperatorTok Op_Equals) *> pure Equal  <|>
    tok (OperatorTok Op_BangE)  *> pure NotEq 

----------------- Utils --------------------

endSemi :: forall a. P a -> P a
endSemi p = p >>= \a -> semiColon *> pure a

endOptSemi :: forall a. P a -> P a
endOptSemi p = p >>= \a -> PC.optional semiColon *> pure a


arrBrackets :: P Unit
arrBrackets = brackets $ pure unit

parens   = PC.between (tok (SymbolTok OpenParen))  (tok (SymbolTok CloseParen))
braces   = PC.between (tok (SymbolTok OpenCurly))  (tok (SymbolTok CloseCurly))
brackets = PC.between (tok (SymbolTok OpenSquare)) (tok (SymbolTok CloseSquare))
angles   = PC.between (tok (OperatorTok Op_LThan))   (tok (OperatorTok Op_GThan))

comma = tok (SymbolTok Comma)
colon     = tok (OperatorTok Op_Colon)
semiColon = tok (SymbolTok SemiColon)
period    = tok (SymbolTok Period)

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

-- langToken :: forall a. (Token -> Maybe a) -> P a
-- langToken test =  token posT showT testT
--     where 
--         showT (L _ t) = show t
--         posT  _ (L p _) _ = Newtype.unwrap p
--         testT (L _ t) = test t

-- tok :: Token -> P Unit 
-- tok t = langToken (\r -> if r == t then Just unit else Nothing)