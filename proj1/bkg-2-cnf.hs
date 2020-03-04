import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import qualified Data.Set as S

data Flag
    = Internal              -- -i
    | PrintCFG              -- -1
    | CFG2CNF               -- -2
    deriving (Eq,Ord,Enum,Show,Bounded)

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

data Nonterminal = Nonterminal Char String deriving (Show)
type Terminal = Char
type Sentence = [Either Terminal Nonterminal]
type Rule = (Nonterminal, Sentence)

data ContextFreeGrammar = CFG { n :: S.Set Nonterminal
                                , e :: S.Set Terminal
                                , p :: S.Set Rule
                                , s :: Nonterminal
                                } deriving (Show)

main = do
    args <- getArgs >>= parseArgs

    case args of
        (_, Nothing) -> putStrLn "Nothing"
        (_, Just file) -> putStrLn $ show file
