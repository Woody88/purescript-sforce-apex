module Language.Apex.Parser.Core where

import Control.Applicative ((<$>))
import Text.Parsing.Parser.String hiding (satisfy)
import Data.Map as M
import Data.HashSet as S

import Language.Apex.AST

-- -- | Type of the Java Parser
-- type JParser a = Parsec [Token] () a

-- -- | Advance token position
-- nextPos :: SourcePos -> t -> [Token] -> SourcePos
-- nextPos _ _ ((_, pos) : _) = pos
-- nextPos pos _ [] = pos

-- -- | Same as Parsec's satisfy, this is for Java token
-- satisfy :: (T -> Bool) -> JParser T
-- satisfy f = tokenPrim show nextPos (\c -> if f (fst c)
--                                           then Just (fst c)
--                                           else Nothing)

-- -- | Fetch next token and advance
-- getT :: JParser T
-- getT = tokenPrim show nextPos (Just . fst)

-- -- | Get the string from a Token context
-- -- | Will fail if the Token does not store a String
-- getSS :: T -> String
-- getSS (TokIdent   s) = s
-- getSS (Keyword    s) = s
-- getSS (Operator   s) = s
-- getSS s              = show s

-- -- | Check the equality between the content of a string storing token
-- --   and a string.
-- (===) :: T -> String -> Bool
-- t === s = getSS t == s

-- isIdentifier (TokIdent _) = True
-- isIdentifier _ = False

-- isKeyword (Keyword _) = True
-- isKeyword _ = False

-- isPeriod Period = True
-- isPeriod _ = False

-- isOperator (Operator _) = True
-- isOperator  _ = False

-- isComma Comma = True
-- isComma _ = False

-- isLSquare LSquare = True
-- isLSquare _ = False

-- isRSquare RSquare = True
-- isRSquare _ = False

-- isLParen LParen = True
-- isLParen _ = False

-- isRParen RParen = True
-- isRParen _ = False

-- isSemiColon SemiColon = True
-- isSemiColon _ = False

-- isDColon DColon = True
-- isDColon _ = False

-- isLBrace LBrace = True
-- isLBrace _ = False

-- isRBrace RBrace = True
-- isRBrace _ = False

-- operator s      = satisfy (\x -> isOperator x && x === s)
-- keyword kwd     = satisfy (\x -> isKeyword x  && x === kwd)

-- lessThan        = operator "<"
-- greaterThan     = operator ">"
-- star            = operator "*"
-- multOp          = star

-- comma           = satisfy isComma
-- lSquare         = satisfy isLSquare
-- rSquare         = satisfy isRSquare
-- lParen          = satisfy isLParen
-- rParen          = satisfy isRParen
-- lBrace          = satisfy isLBrace
-- rBrace          = satisfy isRBrace
-- semiColon       = satisfy isSemiColon
-- dot             = satisfy isPeriod
-- dColon          = satisfy isDColon

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
