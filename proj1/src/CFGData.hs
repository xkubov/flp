{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Data.Char
import Data.List

type Nonterminal = String
type Terminal = String
type Sentence = [String]
data Rule = Rule Nonterminal Sentence deriving (Eq)

instance Show Rule where
    show (Rule nt st) = intercalate "->" [nt, intercalate "" st]

{-|
 - Internal representation of a cont-gree grammar.
 -}
data CFGrammar = CFG {
    nonterminals :: [Nonterminal], -- ^ Set of non-terminals.
    terminals :: [Terminal],       -- ^ Set of terminals.
    initS :: Nonterminal,          -- ^ Initial non-terminal.
    rules :: [Rule]                -- ^ Set of rules.
} deriving (Eq)

instance Show CFGrammar where
    show CFG{..} = unlines $
        [intercalate "," nonterminals,
         intercalate "," terminals,
         initS
        ] ++ map show rules

isTerm :: Char -> Bool
isTerm x = isLower x && x `elem` ['a'..'z']

isTerminal :: String -> Bool
isTerminal x = length x == 1 && all isTerm x

isNonterm :: Char -> Bool
isNonterm x = isUpper x && x `elem` ['A'..'Z']

isNonterminal :: String -> Bool
isNonterminal = not . isTerminal
