module Language.Parser where 

import Data.Either (Either)
import Language.Lexer (lexer)
import Language.Apex.Parser (compilationUnit)
import Language.Apex.Syntax (CompilationUnit)
import Text.Parsing.Parser (ParseError, runParser)

parseCompilationUnit :: String -> Either ParseError CompilationUnit
parseCompilationUnit input = runParser (lexer input) compilationUnit
