{-# LANGUAGE RecordWildCards #-}

module CFGParse where

import Data.Char
import Data.List

import Control.Applicative
import Control.Arrow
import Control.Monad
import Text.Parsec
import Text.Parsec.String

import CFGData

parseCFG :: String -> Either String CFGrammar
parseCFG = validate <=< left show . parse cfgParser ""

cfgParser :: Parser CFGrammar
cfgParser = CFG <$> nontermSetParser        <* newline
                <*> termSetParser           <* newline
                <*> nontermParser <* newline
                <*> ruleSetParser

nontermSetParser :: Parser [Nonterminal]
nontermSetParser = sepBy1 nontermParser comma

nontermParser :: Parser Nonterminal
nontermParser = many1 $ satisfy isNonterm

termSetParser :: Parser [Terminal]
termSetParser = sepBy1 termParser comma

termParser :: Parser Terminal
termParser = many1 $ satisfy isTerm

termNontermParser :: Parser Char
termNontermParser = choice [satisfy isNonterm, satisfy isTerm]

sentenceParser :: Parser Sentence 
sentenceParser = do
    strings <- many1 termNontermParser
    return $ map (:[])strings

ruleSetParser :: Parser [Rule]
ruleSetParser = endBy ruleParser newline

ruleParser :: Parser Rule
ruleParser = Rule <$> nontermParser <* string "->" <*> sentenceParser

comma :: Parser Char
comma = char ','

-- TODO: check loops
-- TODO: check retarded symbols
validate :: CFGrammar -> Either String CFGrammar
validate cfg@CFG{..}
    | allOK = Right cfg
    | not initialInNonterms = Left (
            "nonterminal "++initS++" is not member of ["
                ++ intercalate "," nonterminals ++ "]")
    | not disjointTermsNonterms = Left (
            "sets of terms and nonterms are not disjoint. Common symbols: "
                ++ show (terminals `union` nonterminals))
    | not validRules = Left "specified CFG has invalid rules"
    | not meetInputTermsNonterms = Left "terminals or nonterminals do not meet input criteria from assignment"
    | otherwise = Left "invalid CFG"
  where
    allOK = initialInNonterms && meetInputTermsNonterms && disjointTermsNonterms && validRules
    initialInNonterms = initS `elem` nonterminals
    disjointTermsNonterms = null $ terminals `intersect` nonterminals
    validRules = and [nt `elem` nonterminals | Rule nt _ <- rules]
        && and [x `elem` (terminals `union` nonterminals) | Rule _ alpha <- rules, x <- alpha]
    meetInputTermsNonterms = and [length x == 1 | x <- terminals, x <- nonterminals]

