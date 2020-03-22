{-# LANGUAGE RecordWildCards #-}

module CFGParse where

import Data.Char
import Data.List

import Control.Applicative ((<$>), (<*>), (<$), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Text.Parsec (Parsec, ParseError, parse, choice,
        newline, alphaNum, string, char, satisfy, sepBy1, endBy, many1)
import Text.Parsec.String (Parser)

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
nontermParser = satisfy isNonterminal

termSetParser :: Parser [Terminal]
termSetParser = sepBy1 termParser comma

termParser :: Parser Terminal
termParser = satisfy isTerminal

termNontermParser :: Parser Char
termNontermParser = choice [termParser, nontermParser]

sentenceParser :: Parser Sentence 
sentenceParser = many1 termNontermParser

ruleSetParser :: Parser [Rule]
ruleSetParser = endBy ruleParser newline

ruleParser :: Parser Rule
ruleParser = Rule <$> nontermParser <* (string "->") <*> sentenceParser

comma :: Parser Char
comma = char ','

validate :: CFGrammar -> Either String CFGrammar
validate cfg@CFG{..} = if allOK then Right cfg else
                            if initialInNonterms == False then Left $ "nonterminal "++[initS]++" is not member of [" ++
                                                                    (intercalate "," $ map (\x -> [x]) nonterminals) ++ "]"
                            else if disjointTermsNonterms == False then Left $ "sets of terms and nonterms are not disjoint. Common symbols: "
                                                                                                            ++ show (terminals `union` nonterminals)
                            else if validRules == False then Left $ "specified CFG has invalid rules"
                            else Left "invalid CFG"
  where
    allOK = initialInNonterms && disjointTermsNonterms && validRules
    initialInNonterms = initS `elem` nonterminals
    disjointTermsNonterms = terminals `intersect` nonterminals == []
    validRules = and [nt `elem` nonterminals | Rule nt _ <- rules]
        && and [x `elem` (terminals `union` nonterminals) | Rule _ alpha <- rules, x <- alpha]

