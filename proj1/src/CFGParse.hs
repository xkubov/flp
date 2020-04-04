{-|
Module      : CFGParse
Description : Parser of CFG
Author      : Peter Kubov
Login       : xkubov06
Project     : Functional project: bkg-2-cnf
License     : GPL-3
Year        : 2020

Provides parser and semantic analysis of CFG.
-}

module CFGParse where

import Data.Char
import Data.List

import Control.Applicative
import Control.Arrow
import Control.Monad
import Text.Parsec
import Text.Parsec.String

import CFGData

{-|
 - Main method -> provides control flow of parser of CFG from string.
 - Returnseerror message in string in case of an error.
 -}
parseCFG :: String -> Either String CFGrammar
parseCFG = validate <=< left show . parse cfgParser ""

{-|
 - Definition of how the CFG should looks in String - syntax of CFG.
 -}
cfgParser :: Parser CFGrammar
cfgParser = CFG <$> nontermSetParser  <* newline
                <*> termSetParser     <* newline
                <*> nontermParser     <* newline
                <*> ruleSetParser

{-|
 - Provide parser for nonterminals.
 - Nonterminals are separated by comma without spaces:
 - A,B,C,...
 -}
nontermSetParser :: Parser [Nonterminal]
nontermSetParser = sepBy1 nontermParser comma

{-|
 - Parses nonterminal. Each nonterminal on input must be from [A-Z] set.
 -}
nontermParser :: Parser Nonterminal
nontermParser = do
    nt <- satisfy isNonterm
    return [nt]

{-|
 - Provide parser for terminals.
 - Terminals are separated by comma without spaces:
 - a,b,c,...
 -}
termSetParser :: Parser [Terminal]
termSetParser = sepBy1 termParser comma

{-|
 - Parses terminal. Each nonterminal on input must be from [a-z] set.
 -}
termParser :: Parser Terminal
termParser = do
    nt <- satisfy isTerm
    return [nt]

{-|
 - Parses either terminal or nonterminal.
 -}
termNontermParser :: Parser String
termNontermParser = choice [termParser, nontermParser]

{-|
 - Parses stream of terminals and nonterminals.
 -}
sentenceParser :: Parser Sentence
sentenceParser = many1 termNontermParser

{-|
 - Parses rules. Each rule is on separate line.
 -}
ruleSetParser :: Parser [Rule]
ruleSetParser = endBy ruleParser newline

{-|
 - Parses rules baes on the following form:
 - Nonterm -> (Nonterm or Term)+
 -}
ruleParser :: Parser Rule
ruleParser = Rule <$> nontermParser <* string "->" <*> sentenceParser

{-|
 - Helping definition of comma.
 -}
comma :: Parser Char
comma = char ','

{-|
 - Semantic analysis of the CFG parsing.
 -}
validate :: CFGrammar -> Either String CFGrammar
validate cfg@(CFG nonterminals terminals initS rules)
    | allOK = Right cfg
    | not initialInNonterms = Left (
            "nonterminal "++initS++" is not member of ["
                ++ intercalate "," nonterminals ++ "]")
    | not disjointTermsNonterms = Left (
            "sets of terms and nonterms are not disjoint. Common symbols: "
                ++ show (terminals `union` nonterminals))
    | not validRules = Left "specified CFG has invalid rules"
    | otherwise = Left "invalid CFG"
  where
    allOK = initialInNonterms && disjointTermsNonterms && validRules
    initialInNonterms = initS `elem` nonterminals
    disjointTermsNonterms = null $ terminals `intersect` nonterminals
    validRules = and [nt `elem` nonterminals | Rule nt _ <- rules]
        && and [x `elem` (terminals `union` nonterminals) | Rule _ alpha <- rules, x <- alpha]
