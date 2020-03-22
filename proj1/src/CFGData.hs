{-# LANGUAGE RecordWildCards #-}

module CFGData where

import Data.Char
import Data.List

type Nonterminal = Char
type Terminal = Char
type Sentence = String
data Rule = Rule Nonterminal Sentence deriving (Eq)

instance Show Rule where
    show (Rule nt st) = intercalate "->" [[nt], st]
 
{-|
 - Internal representation of a context-gree grammar.
 -}
data CFGrammar = CFG {
    nonterminals :: [Nonterminal], -- ^ Set of non-terminals.
    terminals :: [Terminal],       -- ^ Set of terminals.
    initS :: Nonterminal,          -- ^ Initial non-terminal.
    rules :: [Rule]                -- ^ Set of rules.
} deriving (Eq)

instance Show CFGrammar where
    show CFG{..} = unlines $
        [intercalate "," $ map (: []) nonterminals,
         intercalate "," $ map (: []) terminals,
         [initS]
        ] ++ map show rules

isTerminal :: Char -> Bool
isTerminal x = isLower x && x `elem` ['a'..'z']

isNonterminal :: Char -> Bool
isNonterminal x = isUpper x && x `elem` ['A'..'Z']
