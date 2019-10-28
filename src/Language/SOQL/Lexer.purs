module Language.SOQL.Lexer where 

import Prelude (map, pure, ($), (*>), (<*), (<<<), (<$>))
import Data.Array as Array
import Data.List as List
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)
import Text.Parsing.Parser (ParseError, runParser)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Language.SOQL.Lexer.Internal
import Language.SOQL.Lexer.Types (P, Token)
import Language.Types (L)

lexSOQL :: String -> List.List (L Token)
lexSOQL = runTokenizer

runTokenizer :: String -> List.List (L Token)
runTokenizer s = List.fromFoldable $ case runParser s tokenize of
                   Left pe    -> []
                   Right toks -> toks
runTokenizer' :: String -> Either ParseError (List.List (L Token))
runTokenizer' s = List.fromFoldable <$> runParser s tokenize 

tokenize :: P (Array (L Token))
tokenize =  javaLexer.whiteSpace *> Array.many nextToken <* PS.eof

nextToken :: P (L Token)
nextToken = javaLexer.lexeme readToken --PC.choice $ map (javaLexer.lexeme <<< PC.try) [longTok, doubleTok, stringTok, boolTok, intTok, opTok, identOrKeyword]
