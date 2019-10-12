module Language.Apex.Lexer.Utils where 

import Prelude
import Data.List (List, someRec)
import Text.Parsing.Parser (position)
import Language.Apex.Lexer.Types (L(..), Token, P)

-- | Match one or more times.
many1 :: forall a. P a -> P (List a)
many1 = someRec

infixr 6 mkToken as <=:

infixr 6 mkTokenWith as <<=:

mkToken :: forall a. Token -> P a -> P (L Token)
mkToken t p = do
    pos <- position
    _ <- p
    pure (L pos t)

mkTokenWith :: forall a. (a -> Token) -> P a -> P (L Token)
mkTokenWith t p = do
    pos <- position
    m <- p
    pure (L pos $ t m)

