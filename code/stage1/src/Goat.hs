module Main where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                               GOAT - COMPILER
--
-- Well-chosen team name:              pli-dream-team-twentee-nineteen
-- Well-chosen team members:
-- * Alan Ung                          alanu
-- * David Stern                       dibstern
-- * Dongge Liu                        donggel
-- * Mariam Shahid                     mariams
-- * Matthew Farrugia-Roberts          farrugiam
--
-- ----------------------------------------------------------------------------

import System.Exit
import System.Environment
import Control.Monad (when)
import Data.List

import GoatLang.Parser (parseProgram)
import GoatLang.PrettyPrint (prettify)

-- ----------------------------------------------------------------------------
-- Program entry-point
-- ----------------------------------------------------------------------------

main :: IO ()
main
  = do
      -- process command-line options:
      (Opts flags sourceFileName) <- checkArgs

      -- -- PARSING PHASE -- --

      -- parse the contents of sourceFileName into an AST, if possible
      sourceCode <- readFile sourceFileName
      let parsed = parseProgram sourceFileName sourceCode
      ast <- case parsed of
        Left err  -> do
            putStrLn $ "Parse error at " ++ show err
            exitWith (ExitFailure 2)
        Right ast -> return ast

      -- handle the parsed program:
      when (flagIsSet 'a' flags) $ do
        print ast
      when (flagIsSet 'p' flags) $ do
        putStr $ prettify ast

      -- -- CODE GENERATION PHASE -- --

      -- compile AST into machine code, and output executable, if possible
      when (null flags || flagIsSet 'x' flags) $ do
        putStrLn "Sorry, can't generate code yet!"

-- ----------------------------------------------------------------------------
-- Processing command-line options
-- ----------------------------------------------------------------------------

-- The Opts structure will hold our options for this program: a list of
-- flags (Chars like 'p' for pretty-printing) and a single source file name:
data Opts = Opts [Flag] FilePath
type Flag = Char

-- List of valid flags
validFlags = "xaph"

-- flagIsSet
-- To interrogate the list of flags (basically `elem')
flagIsSet :: Flag -> [Flag] -> Bool
flagIsSet f flags
  = f `elem` flags

-- checkArgs
-- Parse command line options, either ensuring they have the correct structure
-- or informing the user.
checkArgs :: IO Opts
checkArgs
  = do
      args <- getArgs
      let flags = getFlags args
      when (flagIsSet 'h' flags) $ do
        helpExit
      let invalidFlags = nub flags \\ validFlags
      when (not $ null invalidFlags) $ do
        usageExit ("Invalid flag(s): " ++ (intersperse ',' invalidFlags))
      let arguments = getArguments args
      case arguments of
        [sourceFileName] -> return (Opts flags sourceFileName)
        []               -> usageExit $ "missing required argument: file"
        (_:excess)       -> usageExit $ "excess argument(s): "
                                        ++ intercalate ", " excess

-- getFlags, getArguments
-- helper functions to extract flag characters and positional arguments
-- from the list of all args
getFlags  :: [String] -> [Flag]
getFlags args
  = concat [tail arg | arg <- args, head arg == '-']
getArguments :: [String] -> [String]
getArguments args
  = [arg | arg <- args, head arg /= '-']

-- usageExit
-- does what it says: display a usage message and then exit (with failure)
usageExit :: String -> IO a
usageExit message
  = do
      putStrLn $ "Argument error:\n  " ++ message
      name <- getProgName
      putStrLn $ usageStr name
      exitWith (ExitFailure 1)

-- helpExit
-- Displays a message with usage information and then exits successfully.
helpExit :: IO a
helpExit
  = do
      putStrLn $ concat $ replicate 80 "="
      putStrLn $ "Goat Compiler Usage Information"
      putStrLn $ concat $ replicate 80 "="
      name <- getProgName
      putStrLn $ usageStr name
      exitSuccess

-- usageStr
-- Simply takes the name of the program and returns the formatted usage message
usageStr :: String -> String
usageStr name = "Usage: " ++ name ++ " [option] [file]\n"
        ++ "Options and arguments: \n"
        ++ "  -x    : (or no flags) compile the file & print executable code\n"
        ++ "  -a    : parse the file and print the AST\n"
        ++ "  -p    : parse the file and pretty-print its source code\n"
        ++ "  -h    : print this help message and exit\n"
        ++ "  file  : path to goat (.gt) file containing Goat source code.\n"
        ++ "          Required argument for -x, -a and -p."
