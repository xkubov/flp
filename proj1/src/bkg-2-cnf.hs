module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import CFGData
import CFGParse

{-|
 - Internal representation of options of the program.
 -}
data Flag
    = Internal  -- ^ The `-i` option.
    | PrintCFG  -- ^ The `-1` option.
    | CFG2CNF   -- ^ The `-2` option.
    deriving (Eq,Ord,Enum,Show,Bounded)

{-|
 - List of options containg their description and expected command line flag.
 -}
flags =
    [Option ['i'] []    (NoArg Internal)
        "Prints loaded CFG on stdout."

    ,Option ['1'] []    (NoArg PrintCFG)
        "Pritns CFG without trivial rules on stdout."

    ,Option ['2'] []    (NoArg CFG2CNF)
        "Prints converted CFG to CNF on stdout."
    ]

{-|
 - The 'parseArgs' function to parse arguments.
 - Takes one argument of type '[String]' that represents argv.
 - In case of error exits execution with appropriate message end error code.
 -}
parseArgs :: [String] -> IO(Flag, Maybe String)
parseArgs argv = case getOpt Permute flags argv of
    -- No options -> error
    ([], fs, []) -> do
            hPutStrLn stderr "error: no option provided"
            hPutStrLn stderr (usageInfo header flags)
            exitWith $ ExitFailure 1

    -- More than one file -> error
    (_, files@(fst:scn:rest), []) -> do
            hPutStrLn stderr $ "error: more than one file provided: " ++ intercalate ", " files
            hPutStrLn stderr (usageInfo header flags)
            exitWith $ ExitFailure 1

    -- More than one option -> error
    (options@(fstopt:_), optFile, []) -> do
        let file = if null optFile then Nothing else Just $ head optFile

        -- Duplicit args are fine -> ./bkg-2-cnf -1 -1
        when (length (nub options) > 1) $ do
            hPutStrLn stderr (usageInfo header flags)
            exitWith $ ExitFailure 1

        return (fstopt, file)

    -- Error occoured -> error
    (_, _, errs) -> do
        -- prefix error: to each error. Errors have new line at the end.
        hPutStrLn stderr $ "error: " ++ intercalate "error: " (nub errs) ++ usageInfo header flags
        exitWith (ExitFailure 1)

    where header = "Usage: bkg-2-cnf <options> [input]"

{-|
 - First part of the algorithm 4.5 from TIN.
 - Computes set of reachable nonterminals by following
 - trivial rules.
 -
 - Exapmle of trivial rules is: A->B
 -}
triviallyReachableFrom :: String -> [Rule] -> String
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

transformSymbol :: String -> (String, Maybe ExtRule)
transformSymbol a
    | isExtTerminal a = (nsym, Just (ExtRule nsym [a]))
    | otherwise = (a, Nothing)
    where nsym = a++"'"

transformRule :: ExtRule -> ([ExtRule], [ExtNonterminal])
transformRule rule@(ExtRule _ [_]) = ([rule], [])

transformRule rule@(ExtRule n alpha@[_, _])
    | all isExtNonterminal alpha = ([rule], [])
    | otherwise = ([r | (_, Just r) <- map transformSymbol alpha] -- rules a'->a
                    ++ [ExtRule n transalpha], -- New rule S -> a'A|Aa'|a'a'
                  nub transalpha) -- After transformation alpha will be just nonterms
    where transalpha = [s | (s, _) <- map transformSymbol alpha]

transformRule (ExtRule n (f:alpha')) = (ExtRule n [h', newNt]:rules ++ hr', [h',newNt] ++ nts)
        where (rules, nts) = transformRule (ExtRule newNt alpha')
              newNt = "<"++intercalate "" alpha'++">"
              (h', hr') = case transformSymbol f of
                            (h, Just r) -> (h, [r])
                            _ -> (f, [])

cfgChomskyTransform :: CFGrammar -> ExtCFGrammar
cfgChomskyTransform (CFG nts ts s rules) =
        ExtCFG (map (:[]) nts `union` newnts) (map (:[]) ts) [s] newrules
    where newtransforms = [transformRule $ ExtRule [n] (map (:[]) alpha) | Rule n alpha <- rules]
          newrules = nub [nr | (r, _) <- newtransforms, nr <- r]
          newnts = nub [nn | (_, n) <- newtransforms, nn <- n]

provideAction :: Flag -> CFGrammar -> IO()
provideAction flag
    | flag == Internal = putStr . show
    | flag == PrintCFG = putStr . show . cfgReduceTrivial
    | flag == CFG2CNF = putStr . show . cfgChomskyTransform . cfgReduceTrivial

main = do
    (flag, file) <- getArgs >>= parseArgs

    content <- maybe getContents readFile file

    case parseCFG content of
        Right cfg -> provideAction flag cfg
        Left error -> do
            hPutStrLn stderr $ "error: unable to parse CFG: " ++ unwords (lines error)
            exitWith $ ExitFailure 1
