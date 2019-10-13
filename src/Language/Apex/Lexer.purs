module Language.Apex.Lexer where 

import Prelude (map, pure, ($), (*>), (<*), (<<<))
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Language.Apex.Lexer.Internal
import Language.Apex.Lexer.Types (L, P, Token)

lexJava :: String -> Effect (Array (L Token))
lexJava = runTokenizer

runTokenizer :: String -> Effect (Array (L Token))
runTokenizer s = case runParser s tokenize of
                   Left pe ->  logShow pe *> pure []
                   Right toks -> pure toks

tokenize :: P (Array (L Token))
tokenize =  javaLexer.whiteSpace *> Array.many nextToken <* PS.eof

nextToken :: P (L Token)
nextToken = PC.choice $ map (javaLexer.lexeme <<< PC.try) [longTok, doubleTok, stringTok, boolTok, intTok, opTok, identOrKeyword]

