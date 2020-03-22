{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Control.Applicative ((<$>), (<*>), (<$), (<*), (<|>))
import Control.Arrow (left)
import Control.Monad ((<=<))
import Data.Bool (bool)
import Text.Parsec (Parsec, ParseError, parse, choice,
        newline, alphaNum, string, char, satisfy, sepBy1, endBy, many1)
import Text.Parsec.String (Parser)

import qualified Data.Set as S

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

type Nonterminal = Char
type Terminal = Char
type Sentence = [Char]
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
        [intercalate "," $ map (\x -> [x]) nonterminals,
         intercalate "," $ map (\x -> [x]) terminals,
         [initS]
        ] ++ map show rules

parseCFG :: String -> Either String CFGrammar
parseCFG = validate <=< left show . parse cfgParser ""

cfgParser :: Parser CFGrammar
cfgParser = CFG <$> nontermSetParser        <* newline
                <*> termSetParser           <* newline
                <*> nontermParser <* newline
                <*> ruleSetParser

nontermSetParser :: Parser [Nonterminal]
nontermSetParser = sepBy1 nontermParser comma

-- TODO: more conditions
nontermParser :: Parser Nonterminal
nontermParser = satisfy isUpper 

termSetParser :: Parser [Terminal]
termSetParser = sepBy1 termParser comma

-- TODO: more conditions
termParser :: Parser Terminal
termParser = satisfy isLower

-- TODO: join with term/nonterm parsers
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

--transitiveClosure :: [Rule] -> [Rule]
--transitiveClosure closure
--    | closure == closureUntilNow = closure
--    | otherwise                  = transitiveClosure closureUntilNow
--    where closureUntilNow = nub $ closure ++ [(Rule x z) | (Rule x y) <- closure, (Rule y' z) <- closure, y == y']

cfgReduceTrivial :: CFGrammar -> CFGrammar
cfgReduceTrivial (CFG nonterms terms initS rules) = CFG nonterms terms initS [] -- TODO

cfgChomskyTransform :: CFGrammar -> CFGrammar
cfgChomskyTransform cfg = cfg

provideAction :: Flag -> CFGrammar -> IO()
provideAction flag
    | flag == Internal = putStrLn . show
    | flag == PrintCFG = putStrLn . show . cfgReduceTrivial
    | flag == CFG2CNF = putStrLn . show . cfgChomskyTransform . cfgReduceTrivial

main = do
    (flag, file) <- getArgs >>= parseArgs

    content <- case file of
        Nothing -> getContents
        Just file -> readFile file

    case parseCFG content of
        Right cfg -> provideAction flag cfg
        Left error -> do
            hPutStrLn stderr $ "error: unable to parse CFG: " ++ unwords (lines error)
            exitWith $ ExitFailure 1
