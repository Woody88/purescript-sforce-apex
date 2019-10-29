module Language.Internal where 

import Prelude 
import Control.Monad.State (gets, modify_)
import Data.List (List, uncons)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), isJust)
import Language.Types (L(..))
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser (ParseState(..), ParserT, fail)

langToken :: forall m s a t. Monad m => Show t => (t -> Maybe a) -> ParserT (List (L t)) m a
langToken test =  token posT showT testT
    where 
        showT (L _ t) = show t
        posT  _ (L p _) _ = unwrap p
        testT (L _ t) = test t

tok :: forall m s a t. Monad m => Show t => Eq t => t -> ParserT (List (L t)) m Unit 
tok t = langToken (\r -> if r == t then Just unit else Nothing)

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