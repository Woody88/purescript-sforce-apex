module Language.Apex.Parser where 


import Prelude
import Control.Lazy (fix)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.State (gets, modify_)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.List (List, (:))
import Data.List as List 
import Data.Either 
import Data.Maybe (Maybe(..), isJust, fromMaybe, maybe)
import Data.Newtype as Newtype
import Text.Parsing.Parser.Pos 
import Language.Apex.Lexer 
import Language.Apex.Lexer.Types (L(..), Token(..))
import Language.Apex.Syntax 
import Language.Apex.Syntax.Types 
import Text.Parsing.Parser (Parser, ParseError, position, runParser, fail)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT

type P = Parser (List (L Token))

------------- Top Level parsing -----------------

parseCompilationUnit :: String -> Either ParseError CompilationUnit
parseCompilationUnit input = runParser (lexJava input) compilationUnit

------------- Compilation Unit -----------------
compilationUnit :: P CompilationUnit
compilationUnit = do 
    tds <- list typeDecl
    pure $ CompilationUnit (List.catMaybes tds)

literal :: P Literal
literal = javaToken $ \t -> case t of
    IntegerTok i -> Just (Integer i)
    LongTok    l -> Just (Long l)
    DoubleTok  d -> Just (Double d)
    StringTok  s -> Just (String s)
    BoolTok    b -> Just (Boolean b)
    NullTok      -> Just Null 
    --KeywordTok s -> if s == "null" then Just Null else Nothing
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
    (const Nothing <$> semiColon <|> Just <$> block)

-- Formal parameters

formalParams :: P (List FormalParam)
formalParams = parens $ seplist formalParam comma

formalParam :: P FormalParam
formalParam = do
    ms  <- list modifier
    typ <- type_
    vid <- varDeclId
    pure $ FormalParam ms typ vid


-- Declations 
typeDecl :: P (Maybe TypeDecl)
typeDecl = Just <$> classOrInterfaceDecl <|> const Nothing <$> semiColon

classOrInterfaceDecl :: P TypeDecl
classOrInterfaceDecl = do
    ms <- list modifier
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
    -- tps  <- lopt typeParams
    -- ext  <- optMaybe extends 
    -- imp  <- lopt implements
    body <- classBody
    pure $ \ms -> ClassDecl ms i mempty Nothing mempty body
    -- pure $ \ms -> ClassDecl ms i tps (ext >>= List.head) mempty body

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
classBodyStatements = List.catMaybes <$> list (fix $ \_ -> classBodyStatement)

classBodyStatement :: P (Maybe Decl)
classBodyStatement =
    (PC.try $ do
       _ <- list1 semiColon
       pure Nothing) <|>
    (PC.try $ do
       mst <- bopt (tok KW_Static)
       blk <- block
       pure $ Just $ InitDecl mst blk) <|>
    (do ms  <- list modifier
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
    mec <- optMaybe (PC.try explConstrInv)
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
    (do pri <- primary
        period
        tas <- lopt refTypeArgs
        tok KW_Super
        as  <- args
        pure $ PrimarySuperInvoke pri tas as)

args :: P (List Argument)
args = parens $ seplist expression comma

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
    do ms  <- list modifier
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
    <|> Annotation <$> annotation

annotation :: P Annotation
annotation = do 
    annName <- tok Op_AtSign *> name
    PC.try (parens evlist >>= \annKV -> pure $ NormalAnnotation { annName,  annKV }) <|> (pure $ MarkerAnnotation { annName })


evlist :: P (List (Tuple Ident ElementValue))
evlist = seplist1 elementValuePair comma

elementValuePair :: P (Tuple Ident ElementValue)
elementValuePair = Tuple <$> ident <* tok Op_Equal <*> elementValue

elementValue :: P ElementValue
elementValue = EVVal <$> InitExp <$> infixExp

------------ Variable declarations ----------------

localVarDecl :: P (Tuple3 (List Modifier) Type (List VarDecl))
localVarDecl = do
    ms <- list modifier
    typ <- type_
    vds <- varDecls
    pure $ Tuple3 ms typ vds

varDecls :: P (List VarDecl)
varDecls = seplist1 varDecl comma 

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

-- more to be added
expression :: P Exp
expression = infixExp 

primary :: P Exp 
primary = flip PC.withErrorMessage "primary expression" $ PC.choice $ map PC.try 
        [ (Lit <$> literal)
        ] 

infixExp :: P Exp 
infixExp = unaryExp 

unaryExp :: P Exp 
unaryExp = postfixExp 

postfixExp :: P Exp 
postfixExp = postfixExpNES

postfixExpNES :: P Exp 
postfixExpNES = primary

----------------------------------------------------------------------------
-- Statements

block :: P Block
block = braces $ Block <$> list blockStmt

blockStmt :: P BlockStmt
blockStmt = do
    (Tuple3 m t vds) <- endSemi $ localVarDecl 
    pure $ LocalVars m t vds

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

-- return type of a methodx
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

javaToken :: forall a. (Token -> Maybe a) -> P a
javaToken f = do
    (L _ t) <- PT.token (\(L pos _) -> Newtype.unwrap pos)
    p <- position
    maybe (fail $ "error parsing toking: " <> show t <> ": " <> show p) pure $ f t

tok :: Token -> P Unit
tok t = javaToken (\r -> if r == t then Just unit else Nothing)

optMaybe :: forall a. P a -> P (Maybe a)
optMaybe = PC.optionMaybe 

bopt :: forall a. P a -> P Boolean
bopt p = optMaybe p >>= \ma -> pure $ isJust ma 

lopt :: forall a. P (List a) -> P (List a)
lopt p = do mas <- optMaybe p
            case mas of
             Nothing -> pure mempty
             Just as -> pure as