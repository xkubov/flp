{-|
Module      : Main
Description : Implementation of algorithms.
Author      : Peter Kubov
Login       : xkubov06
Project     : Functional project: bkg-2-cnf
License     : GPL-3
Year        : 2020

This module contains implementation of algorithms from TIN.
-}

module CFGAlgs where

import Data.List

import CFGData

{-|
 - First part of the algorithm 4.5 from TIN.
 - Computes set of reachable nonterminals by following
 - trivial rules.
 -
 - Exapmle of trivial rules is: A->B
 -}
triviallyReachableFrom :: [String] -> [Rule] -> [String]
triviallyReachableFrom closure rules
    | closure == closureUntilNow = closure
    | otherwise                  = triviallyReachableFrom closureUntilNow rules
    where closureUntilNow = nub $ closure
            ++ [c | Rule b [c] <- rules, isNonterminal c, b `elem` closure]

{-|
 - Second part of the algorithm 4.5 from TIN.
 - Transforms CFG into CFG with no trivial rules.
 -
 - Exapmle of trivial rules is: A->B
 -}
cfgReduceTrivial :: CFGrammar -> CFGrammar
cfgReduceTrivial (CFG nts ts s rules) = CFG nts ts s newrules
    where newrules = [Rule a alpha |
            -- Take only nontrivial rules: first symbol isNonterminal => lenght > 1
            Rule b alpha@(f:_) <- rules, isTerminal f || ((>1).length) alpha,
            a <- nts,
            b `elem` triviallyReachableFrom [a] rules]

{-|
 - Transforms symbol based on the algoritm 4.7 TIN.
 -
 - Nonterminals are not transformed but are added
 - for convinence.
 -
 - Terminals are transformed like this:
 -     a => a'
 -     Plus rules a' -> a are added too.
 -}
transformSymbol :: String -> (String, Maybe Rule)
transformSymbol a
    | isTerminal a = (nsym, Just (Rule nsym [a]))
    | otherwise = (a, Nothing)
    where nsym = a++"'"

{-|
 - Transforms rules baed on the algorithm 4.7 from TIN.
 - Each case is docummented separately.
 -}
transformRule :: Rule -> ([Rule], [Nonterminal])

{-|
 - Algorithm expects nontrivial rules. if rule
 - A -> a exists than a is terminal and this
 - comforts to Chomsky Form.
 -}
transformRule rule@(Rule _ [_]) = ([rule], [])

{-|
 - All rules A -> XY
 - if XY are all nonterms then it comforts Chomsky Form.
 - if either X or Y or both are terms then those should be transformed.
 -}
transformRule rule@(Rule n alpha@[_, _])
    | all isNonterminal alpha = ([rule], [])
    | otherwise =
        (rulesCreatedByTransformation -- rules a'->a
            ++ [Rule n transalpha] -- New rule S -> a'A|Aa'|a'a'
        , nub transalpha) -- After transformation alpha will be just nonterms
    where transalpha = [s | (s, _) <- map transformSymbol alpha]
          rulesCreatedByTransformation = [r | (_, Just r) <- map transformSymbol alpha]

{-|
 - All rules that consist of more than 3 symbols on right side must be recursively
 - transformed.
 - X -> XaYZ
 - ---------
 - X -> X<aYZ>
 - <aYZ> -> a'<YZ>
 - a' -> a
 - <YZ> -> YZ
 -}
transformRule (Rule n (f:alpha')) = (Rule n [h', newNt]:rules ++ hr', [h',newNt] ++ nts)
    where (rules, nts) = transformRule (Rule newNt alpha') -- recursively transform nnew rule.
          newNt = "<"++intercalate "" alpha'++">" -- new nonterminal from sentence: abc -> <ABC>
          (h', hr') = case transformSymbol f of
                        (h, Just r) -> (h, [r])
                        _ -> (f, [])

{-|
 - Transforms CFG on input to CFG in Chomsky form.
 - Grammar on input MUST NOT contain any trivial rules.
 -
 - Transformation is based on algorithm 4.7 in TIN.
 -}
cfgChomskyTransform :: CFGrammar -> CFGrammar
cfgChomskyTransform (CFG nts ts s rules) =
        CFG ( nts `union` newnts) ts s newrules
    where newtransforms = [transformRule r | r <- rules]
          newrules = nub [nr | (r, _) <- newtransforms, nr <- r]
          newnts = nub [nn | (_, n) <- newtransforms, nn <- n]
