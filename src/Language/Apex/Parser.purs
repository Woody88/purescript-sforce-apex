module Language.Apex.Parser where 


import Prelude
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Tuple (Tuple(..))
import Data.List (List, (:))
import Data.List as List 
import Data.Either 
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as Newtype
import Language.Apex.Lexer 
import Language.Apex.Lexer.Types (L(..), Token(..))
import Language.Apex.Syntax 
import Language.Apex.Syntax.Types 
import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Token as PT

type P = Parser (List (L Token))

------------- Top Level parsing -----------------

parseCompilationUnit :: String -> Either ParseError (Tuple Type (List VarDecl))
parseCompilationUnit input =  runParser (lexJava input) localVarDecl
    
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

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

-- Modifiers

-- modifier :: P Modifier
-- modifier =
--         tok KW_Public      >> return Public
--     <|> tok KW_Protected   >> return Protected
--     <|> tok KW_Private     >> return Private
--     <|> tok KW_Abstract    >> return Abstract
--     <|> tok KW_Static      >> return Static
--     <|> tok KW_Strictfp    >> return StrictFP
--     <|> tok KW_Final       >> return Final
--     <|> tok KW_Native      >> return Native
--     <|> tok KW_Transient   >> return Transient
--     <|> tok KW_Volatile    >> return Volatile
--     <|> tok KW_Synchronized >> return Synchronized_
--     <|> Annotation <$> annotation

------------ Variable declarations ----------------
localVarDecl :: P (Tuple Type (List VarDecl))
localVarDecl = do
    typ <- type_
    vds <- varDecls
    pure $ Tuple typ vds

varDecls :: P (List VarDecl)
varDecls = seplist1 varDecl comma 

varDecl :: P VarDecl 
varDecl = do
    vdi <- varDeclId
    vi <- optMaybe $ tok Op_Equal *> varInit 
    pure $ VarDecl vdi vi 

varDeclId :: P VarDeclId 
varDeclId = do 
    id <- ident
    pure $ VarDeclArray $ VarId id

varInit :: P VarInit
varInit = InitExp <$> expression 

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

---------------- Types ---------------------

type_ :: P Type
type_ = PrimType <$> primType

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

comma = tok Comma

javaToken :: forall a. (Token -> Maybe a) -> P a
javaToken f = PC.try $ do
    (L _ t) <- PT.token (\(L pos _) -> Newtype.unwrap pos)
    maybe (fail "error parsing toking") pure $ f t

tok :: Token -> P Unit
tok t = javaToken (\r -> if r == t then Just unit else Nothing)

optMaybe :: forall a. P a -> P (Maybe a)
optMaybe = PC.optionMaybe 