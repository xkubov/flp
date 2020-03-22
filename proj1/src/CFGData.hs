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

type ExtNonterminal = String
type ExtTerminal = String
type ExtSentence = [String]
data ExtRule = ExtRule ExtNonterminal ExtSentence deriving (Eq)

instance Show ExtRule where
    show (ExtRule nt st) = intercalate "->" [nt, intercalate "" st]

{-|
 - Internal representation of a context-gree grammar.
 -}
data ExtCFGrammar = ExtCFG {
    extNonterminals :: [ExtNonterminal], -- ^ Set of non-terminals.
    extTerminals :: [ExtTerminal],       -- ^ Set of terminals.
    extInitS :: ExtNonterminal,          -- ^ Initial non-terminal.
    extRules :: [ExtRule]                -- ^ Set of rules.
} deriving (Eq)

instance Show ExtCFGrammar where
    show ExtCFG{..} = unlines $
        [intercalate "," extNonterminals,
         intercalate "," extTerminals,
         extInitS
        ] ++ map show extRules

isTerminal :: Char -> Bool
isTerminal x = isLower x && x `elem` ['a'..'z']

isNonterminal :: Char -> Bool
isNonterminal x = isUpper x && x `elem` ['A'..'Z']
