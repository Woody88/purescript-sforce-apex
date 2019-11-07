module Language.Lexer where 

import Prelude 
import Data.Either (Either(..))
import Data.List (List, fromFoldable, some, many)
import Language.Types (L, Token, P)
import Language.Core 
import Text.Parsing.Parser (ParseError, runParser)
import Text.Parsing.Parser.String (eof)

lexer :: String -> List (L Token)
lexer s = fromFoldable $ case runParser s tokenize of
    Left pe    -> mempty
    Right toks -> toks

runTokenizer' :: String -> Either ParseError (List (L Token))
runTokenizer' s = runParser s tokenize 

tokenize :: P (List (L Token))
tokenize =  javaLexer.whiteSpace *> many nextToken <* eof

nextToken :: P (L Token)
nextToken = javaLexer.lexeme readToken 