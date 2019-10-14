module Language.Apex.Parser where 


import Prelude

import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as Newtype
import Language.Apex.Lexer.Types (L(..), Token(..))
import Language.Apex.Syntax (Exp(..), VarInit(..))
import Language.Apex.Syntax.Types (Ident(..), Literal(..))
import Text.Parsing.Parser (Parser, fail)
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
varInit :: P VarInit
varInit = InitExp <$> expression 

-- more to be added
expression :: P Exp
expression = primary 

primary :: P Exp 
primary = flip PC.withErrorMessage "primary expression" $ PC.choice $ map PC.try 
        [ (Lit <$> literal)
        ] 

javaToken :: forall a. (Token -> Maybe a) -> P a
javaToken f = PC.try $ do
    (L _ t) <- PT.token (\(L pos _) -> Newtype.unwrap pos)
    maybe (fail "error parsing toking") pure $ f t

tok :: Token -> P Unit
tok t = javaToken (\r -> if r == t then Just unit else Nothing)