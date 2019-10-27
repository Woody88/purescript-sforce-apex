module Language.Apex.Lexer.Utils where 

import Prelude

import Control.Monad.State (gets, modify_)
import Data.List (List, someRec)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (drop, length, toLower)
import Language.Apex.Lexer.Types (Token, P)
import Language.Types (L(..), Pos(..))
import Text.Parsing.Parser (ParseState(..), fail, position)
import Text.Parsing.Parser.Pos (updatePosString)
import Text.Parsing.Parser.String (indexOf)


-- | Match the specified string - case insensitive .
istring :: String -> P String
istring str = do
  input <- gets \(ParseState input _ _) -> input
  case indexOf (wrap str) (toLower input) of
    Just 0 -> do
      modify_ \(ParseState _ position _) ->
        ParseState (drop (length str) input)
                   (updatePosString position str)
                   true
      pure str
    _ -> fail ("Expected " <> show str)

-- | Match one or more times.
many1 :: forall a. P a -> P (List a)
many1 = someRec

infixr 6 mkToken as <=:

infixr 6 mkTokenWith as <<=:

mkToken :: forall a. Token -> P a -> P (L Token)
mkToken t p = do
    pos <- position
    _ <- p
    pure (L (Pos pos) t)

mkTokenWith :: forall a. (a -> Token) -> P a -> P (L Token)
mkTokenWith t p = do
    pos <- position
    m <- p
    pure (L (Pos pos) $ t m)

