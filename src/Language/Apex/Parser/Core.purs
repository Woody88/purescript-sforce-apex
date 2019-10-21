module Language.Apex.Parser.Core where

import Language.Apex.AST
import Prelude
import Text.Parsing.Parser.String hiding (satisfy)

import Control.Applicative ((<$>))
import Control.Monad.State (gets, modify_)
import Data.HashSet as S
import Data.List (List)
import Data.List as List
import Data.List.Partial as LP
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Text.Parsing.Parser (ParseState(..), Parser, fail)
import Text.Parsing.Parser.Pos (Position(..))

-- -- | Type of the Java Parser
type P a = Parser (List Token) a

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

-- javaToken' :: forall a. (Token -> Maybe a) -> P a
-- javaToken' test =  token posT showT testT
--     where 
--         showT (L _ t) = show t
--         posT  _ (L p _) _ = Newtype.unwrap p
--         testT (L _ t) = test t


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

-- fromModifierTable :: S.HashSet String -> JParser Modifier
-- fromModifierTable wl = 
--         (\k -> modifierTable M.! getSS k) 
--         <$> satisfy (isModifierOf wl)

-- isModifierOf whiteList kwd = isKeyword kwd &&
--                     (getSS kwd `S.member` whiteList)

-- (|>>) :: JParser a -> JParser (a -> a) -> JParser a
-- (|>>) left suffix = do
--     l <- left
--     s <- option [] (many1 suffix)
--     return $ foldl (\x y -> y x) l s

-- -- | Stores valid keywords for class modifier.
-- modifierTable :: M.Map String Modifier
-- modifierTable = M.fromList [
--     ("public", Public), ("protected", Protected), ("private", Private),
--     ("static", Static), ("final", Final), ("strictfp", StrictFP),
--     ("abstract", Abstract), ("volatile", Volatile), ("transient", Transient),
--     ("synchronized", Synchronized), ("native", Native), ("interface", Interface),
--     ("default", Default)]

-- -- | Stores valid keywords for class modifier.
-- classModifierTable :: S.HashSet String
-- classModifierTable = S.fromList
--     ["public", "protected", "private", "static", "strictfp", "abstract",
--      "final"]

-- -- | Stores valid keywords for interface modifier.
-- interfaceModifierTable :: S.HashSet String
-- interfaceModifierTable = S.fromList
--     ["public", "protected", "private" , "static", "strictfp" , "abstract"]

-- -- | Stores valid keywords for interface method modifier.
-- interfaceMethodModifierTable :: S.HashSet String
-- interfaceMethodModifierTable = S.fromList
--     ["public", "static", "strictfp" , "abstract", "default"]

-- -- | Stores valid keywords for field modifier.
-- fieldModifierTable :: S.HashSet String
-- fieldModifierTable = S.fromList
--     ["public", "protected", "private", "static", "final", "transient",
--      "volatile"]

-- -- | Stores valid keywords for method modifier.
-- methodModifierTable :: S.HashSet String
-- methodModifierTable = S.fromList
--     ["public", "protected", "private", "static", "final", "transient",
--      "volatile", "synchronized", "native", "strictfp"]

-- -- | Stores valid keywords for constructor modifier.
-- constructorModifierTable :: S.HashSet String
-- constructorModifierTable = S.fromList ["public", "protected", "private"]

-- -- | Stores valid keywords for constants modifier.
-- constantModifierTable :: S.HashSet String
-- constantModifierTable = S.fromList ["public", "static", "final"]
