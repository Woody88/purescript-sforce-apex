module Language.Internal where 

import Prelude 
import Control.Alt ((<|>))
import Control.Monad.State (gets, modify_)
import Control.Monad.Rec.Class (class MonadRec)
import Data.List (List, (:), uncons, singleton, someRec)
import Data.Newtype (unwrap, wrap)
import Data.Maybe (Maybe(..), isJust)
import Data.String (drop, length, toLower)
import Language.Types (L(..), Token(..), P, Pos(..))
import Text.Parsing.Parser.Combinators (optionMaybe, try, option)
import Text.Parsing.Parser (ParseState(..), ParserT, fail, position)
import Text.Parsing.Parser.Pos (updatePosString)
import Text.Parsing.Parser.String (indexOf)

langToken :: forall m s a t. Monad m => Show t => (t -> Maybe a) -> ParserT (List (L t)) m a
langToken test =  token posT showT testT
    where 
        showT (L _ t) = show t
        posT  _ (L p _) _ = unwrap p
        testT (L _ t) = test t

tok :: forall m s a t. Monad m => Show t => Eq t => t -> ParserT (List (L t)) m Unit 
tok t = langToken (\r -> if r == t then Just unit else Nothing)

tok' :: forall m. Monad m => String -> ParserT (List (L Token)) m Unit
tok' t = token
    where 
        token = getIdent >>= tokenName
        tokenName s = if toLower s == t then pure unit else fail ("unexpected " <> t <> " token")

getIdent :: forall m. Monad m => ParserT (List (L Token)) m String
getIdent = langToken $ \t -> case t of 
    IdentTok x -> Just x
    _       -> Nothing

token nextpos showt test = do
    input <- gets \(ParseState input _ _) -> input
    case uncons input of
      Nothing -> fail "Unexpected EOF"
      Just {head,tail} -> do
        case test head of 
            Nothing -> fail $ "Unexpected token: " <> showt head
            Just  x -> do
                _ <- modify_ \(ParseState _ pos _) -> ParseState tail (nextpos pos head tail) true
                pure x

optMaybe :: forall m s a. Monad m => ParserT s m a -> ParserT s m (Maybe a)
optMaybe = optionMaybe 

bopt :: forall m s a. Monad m => ParserT s m a -> ParserT s m Boolean
bopt p = optMaybe p >>= \ma -> pure $ isJust ma 

lopt :: forall m s a. Monad m => ParserT s m (List a) -> ParserT s m (List a)
lopt p = do 
    mas <- optMaybe p
    case mas of
        Nothing -> pure mempty
        Just as -> pure as

empty :: forall m s a. Monad m => ParserT s m Unit
empty = pure unit

seplist :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
seplist p sep = option mempty $ seplist1 p sep

seplist1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
seplist1 p sep = do 
    a <- p
    try (loopList a) <|> (pure $ singleton a)
    where 
        loopList a' = do 
            _ <- sep
            as <- seplist1 p sep
            pure (a':as)

list :: forall m s a. Monad m => MonadRec m => ParserT s m a -> ParserT s m (List a)
list = option mempty <<< list1

list1 :: forall m s a. Monad m => MonadRec m => ParserT s m a -> ParserT s m (List a)
list1 = someRec

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
