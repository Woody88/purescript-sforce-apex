module Language.Apex.Parser where 

-- import Prelude
-- import Control.Alt ((<|>))
-- import Control.Lazy (fix)
-- import Data.Either (either)
-- import Data.Int (toNumber)
-- import Text.Parsing.Parser (Parser)
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
