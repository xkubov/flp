{-|
Module      : Main
Description : Main module of the project.
Author      : Peter Kubov
Login       : xkubov06
Project     : Functional project: bkg-2-cnf
License     : GPL-3
Year        : 2020

Contains main control flow of the application.
-}

module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import CFGAlgs
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
 - Provides action with CFG on input based on input flag.
 -}
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
