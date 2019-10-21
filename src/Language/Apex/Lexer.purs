module Language.Apex.Lexer where


import Language.Apex.AST
import Language.Apex.Lexer.Internal
import Prelude

import Control.Applicative ((<*), (*>))
import Data.Array as Array
import Data.Either (Either, either)
import Data.List (List)
import Data.List as List
import Text.Parsing.Parser (ParseError(..), Parser, runParser)
import Text.Parsing.Parser.Combinators (try, choice)
import Text.Parsing.Parser.String (eof)

lexJava :: String -> List Token
lexJava = runTokenizer

runTokenizer :: String -> List Token
runTokenizer = either (const mempty) identity <<< runTokenizer'

runTokenizer' :: String -> Either ParseError (List Token)
runTokenizer' s = List.fromFoldable <$> runParser s tokenize 

tokenize :: Parser String (Array Token)
tokenize = javaLexer.whiteSpace *> Array.many nextToken <* eof

nextToken :: Parser String Token
nextToken = choice $ map (javaLexer.lexeme <<< try)
        [ comm, period, at, lParen, rParen, lSquare, rSquare, lBrace, rBrace
        , semiColon, tokNull, tokBool, tokString, tokLong
        , tokDouble, tokInt, op, identOrKeyword
        ]