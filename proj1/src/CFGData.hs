{-|
Module      : CFGData
Description : Defines Context Free Grammar data type.
Author      : Peter Kubov
Project     : Functional project: bkg-2-cnf
License     : GPL-3
Year        : 2020

This module defines/exports definitionof CFG - Context Free Grammar
and all the types that are related.
-}

module CFGData where

import Data.Char
import Data.List

{-|
 - Internal representation of a cont-gree grammar.
 -}
data CFGrammar = CFG {
    nonterminals :: [Nonterminal], -- ^ Set of non-terminals.
    terminals :: [Terminal],       -- ^ Set of terminals.
    initS :: Nonterminal,          -- ^ Initial non-terminal.
    rules :: [Rule]                -- ^ Set of rules.
} deriving (Eq)

{-|
 - Defines how grammar willbe showed.
 -}
instance Show CFGrammar where
    show (CFG nonterminals terminals initS rules) = unlines $ [
            intercalate "," nonterminals,
            intercalate "," terminals,
            initS
        ] ++ map show rules

{-|
 - As we want to extend nonterminals with special
 - characters typedef to String will be sufficient.
 -}
type Nonterminal = String

{-|
 - For convinient work with nonterminals/terminals
 - terninals should be of same type as Nonterminals.
 -}
type Terminal = String

{-|
 - Sentence consists of combination of terms/nonterms.
 -}
type Sentence = [String]

{-|
 - Rule `subset` Nonterminal x Sentence
 -}
data Rule = Rule Nonterminal Sentence deriving (Eq)

{-|
 - Define how rules will be showed.
 -}
instance Show Rule where
    show (Rule nt st) = intercalate "->" [nt, intercalate "" st]

{-|
 - Helping function that defines terminal.
 -}
isTerm :: Char -> Bool
isTerm x = isLower x && x `elem` ['a'..'z']

{-|
 - Answers wheter symbol on input is terminal.
 -}
isTerminal :: String -> Bool
isTerminal x = length x == 1 && all isTerm x

{-|
 - Helping function that defines nonterminal.
 -}
isNonterm :: Char -> Bool
isNonterm x = isUpper x && x `elem` ['A'..'Z']

{-|
 - Answers wheter symbol in inputis nonterminal.
 -}
isNonterminal :: String -> Bool
isNonterminal = not . isTerminal
