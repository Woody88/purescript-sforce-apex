module Language.Apex.Parser.Core where

import Language.Apex.AST
import Prelude
import Text.Parsing.Parser.String hiding (satisfy)

import Control.Applicative ((<$>))
import Control.Monad.State (gets, modify_)
import Data.HashSet as S
import Data.List (List, foldl)
import Data.List as List
import Data.List.Partial as LP
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Text.Parsing.Parser (ParseState(..), Parser, fail)
import Text.Parsing.Parser.Combinators (option)
import Text.Parsing.Parser.Pos (Position(..))

-- -- | Type of the Java Parser
type P a = Parser (List Token) a

infixr 6 pipeParseR as |>>
    
token nextpos showt test = do
    input <- gets \(ParseState input _ _) -> input
    case List.uncons input of
      Nothing -> fail "Unexpected EOF"
      Just {head,tail} -> do
        case test head of 
            Nothing -> fail $ "Unexpected token: " <> showt head
            Just  x -> do
                _ <- modify_ \(ParseState _ pos _) -> ParseState tail (nextpos pos head tail) true
                
                pure x

pipeParseR :: forall a. P a -> P (a -> a) -> P a
pipeParseR left suffix = do
    l <- left
    s <- option mempty (List.someRec suffix)
    pure $ foldl (\x y -> y x) l s


-- | Advance token position
nextPos :: forall t. Position -> t -> List Token -> Position
nextPos pos tk tkl 
    | List.null tkl = pos 
    | Just (Tuple _ p) <- List.head tkl = p
    | otherwise = pos 

-- | Same as Parsec's satisfy, this is for Apex token
satisfy :: (T -> Boolean) -> P T
satisfy f = token nextPos show  
    (\c -> if f (fst c)
           then Just (fst c)
           else Nothing)

-- -- | Fetch next token and advance
getT :: P T
getT = token nextPos show (Just <<< fst)

-- -- | Get the string from a Token context
-- -- | Will fail if the Token does not store a String
getSS :: T -> String
getSS (TokIdent   s) = s
getSS (Keyword    s) = s
getSS (Operator   s) = s
getSS s              = show s

-- -- | Check the equality between the content of a string storing token
-- --   and a string.
isToken :: T -> String -> Boolean
isToken t s = getSS t == s

infixr 4 isToken as === 

isIdentifier (TokIdent _) = true
isIdentifier _ = false

isKeyword (Keyword _) = true
isKeyword _ = false

isPeriod Period = true
isPeriod _ = false

isOperator (Operator _) = true
isOperator  _ = false

isComma Comma = true
isComma _ = false

isLSquare LSquare = true
isLSquare _ = false

isRSquare RSquare = true
isRSquare _ = false

isLParen LParen = true
isLParen _ = false

isRParen RParen = true
isRParen _ = false

isSemiColon SemiColon = true
isSemiColon _ = false

isLBrace LBrace = true
isLBrace _ = false

isRBrace RBrace = true
isRBrace _ = false

operator s      = satisfy (\x -> isOperator x && x === s)
keyword kwd     = satisfy (\x -> isKeyword x && x === kwd)

lessThan        = operator "<"
greaterThan     = operator ">"
star            = operator "*"
multOp          = star

comma           = satisfy isComma
lSquare         = satisfy isLSquare
rSquare         = satisfy isRSquare
lParen          = satisfy isLParen
rParen          = satisfy isRParen
lBrace          = satisfy isLBrace
rBrace          = satisfy isRBrace
semiColon       = satisfy isSemiColon
dot             = satisfy isPeriod

fromModifierTable :: S.HashSet String -> P Modifier
fromModifierTable wl = do 
    m <- satisfy (isModifierOf wl)
    let md = M.lookup (getSS m) modifierTable
    maybe (fail "expected a modifier") pure md 

isModifierOf whiteList kwd = 
    isKeyword kwd && (getSS kwd `S.member` whiteList)

-- | Stores valid keywords for class modifier.
modifierTable :: M.Map String Modifier
modifierTable = M.fromFoldable [
    ("public" /\ Public), ("protected" /\ Protected), ("private" /\ Private),
    ("static" /\ Static), ("final" /\ Final), ("virtual" /\ Virtual), ("override" /\ Override),
    ("abstract" /\ Abstract), ("extends" /\ Extends), ("transient" /\ Transient),
    ("with sharing" /\ WithShare), ("without sharing" /\ WithoutShare), ("interface" /\ Interface),
    ("inherit sharing" /\ InheritShare), ("global" /\ Global)]

-- | Stores valid keywords for class modifier.
classModifierTable :: S.HashSet String
classModifierTable = S.fromFoldable
    ["public", "protected", "private", "static", "virtual", "abstract", "global",
     "final", "abstract", "with sharing", "without sharing", "inherit sharing"]

-- | Stores valid keywords for interface modifier.
interfaceModifierTable :: S.HashSet String
interfaceModifierTable = S.fromFoldable ["public"]

-- | Stores valid keywords for interface method modifier.
interfaceMethodModifierTable :: S.HashSet String
interfaceMethodModifierTable = interfaceModifierTable

-- | Stores valid keywords for field modifier.
fieldModifierTable :: S.HashSet String
fieldModifierTable = S.fromFoldable
    ["public", "protected", "private", "static", "final", "transient", "global"]

    -- | Stores valid keywords for method modifier.
methodModifierTable :: S.HashSet String
methodModifierTable = S.fromFoldable
    ["public", "protected", "private", "static", "final", "transient", "global", 
     "override"]

-- | Stores valid keywords for constructor modifier. 
constructorModifierTable :: S.HashSet String
constructorModifierTable = S.fromFoldable ["public", "protected", "private", "global"]

-- | Stores valid keywords for constants modifier.
constantModifierTable :: S.HashSet String
constantModifierTable = S.fromFoldable ["public", "global", "static", "final"]
