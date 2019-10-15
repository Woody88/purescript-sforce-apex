module Language.Apex.Parser where 


import Prelude
import Control.Apply ((*>))
import Data.List (List)
import Data.Either 
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as Newtype
import Language.Apex.Lexer 
import Language.Apex.Lexer.Types (L(..), Token(..))
import Language.Apex.Syntax 
import Language.Apex.Syntax.Types (Ident(..), Literal(..))
import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Token as PT

-- import Text.Parsing.Parser.String (char, eof)
-- import Text.Parsing.Parser.Combinators (sepBy1)
-- import Language.Syntax (Exp(..))
-- import Language.Lexer as Lx

-- program :: Parser String Exp
-- program = Lx.lexer.whiteSpace *> expression <* eof
--   where
--     sign = (char '-' *> pure negate) <|> (char '+' *> pure identity) <|> pure identity
--     number = Number <$> (sign <*> (either toNumber identity <$> Lx.lexer.naturalOrFloat))
--     string = String <$> Lx.lexer.stringLiteral  
--     list expr = List <$> Lx.lexer.brackets (sepBy1 expr Lx.lexer.comma)
--     assignment expr = Assignment <$> Lx.lexer.identifier <*> (Lx.lexer.symbol "=" *> expr)
--     expression = fix \expr -> string <|> number <|> list expr <|> assignment expr

type P = Parser (List (L Token))

------------- Top Level parsing -----------------
-- parseCompilationUnit :: String -> Either ParseError VarDecl 
-- parseCompilationUnit input =  do 
--         tokens <- lexJava input
        
--         runParser input varDecl
    

literal :: P Literal
literal = javaToken $ \t -> case t of
    IntTok     i -> Just (Int i)
    LongTok    l -> Just (Long l)
    DoubleTok  d -> Just (Double d)
    StringTok  s -> Just (String s)
    BoolTok    b -> Just (Boolean b)
    --KeywordTok s -> if s == "null" then Just Null else Nothing
    _ -> Nothing

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

----------------------------------------------------------------------------
-- Variable declarations
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

----------------- Utils --------------------

javaToken :: forall a. (Token -> Maybe a) -> P a
javaToken f = PC.try $ do
    (L _ t) <- PT.token (\(L pos _) -> Newtype.unwrap pos)
    maybe (fail "error parsing toking") pure $ f t

tok :: Token -> P Unit
tok t = javaToken (\r -> if r == t then Just unit else Nothing)

optMaybe :: forall a. P a -> P (Maybe a)
optMaybe = PC.optionMaybe 