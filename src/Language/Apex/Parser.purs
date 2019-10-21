
module Language.Apex.Parser where

import Text.Parsing.Parser
import Language.Apex.Lexer
import Language.Apex.Parser.Internal

-- parseProgram = parseJava javaProgram
-- parseTokens = parse javaProgram ""
-- parseJava p s =
--         let s' = runTokenizer s
--         in case parse p "" s' of
--                Right r -> r
--                Left l -> error (show l)
