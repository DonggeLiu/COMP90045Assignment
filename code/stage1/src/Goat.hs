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
import Control.Monad (when, unless)
import Data.List (nub, intersperse, intercalate, (\\))

import Util.ColourParTTY

import GoatLang.Parser (parseProgram)
import GoatLang.PrettyPrint (prettify)
import GoatLang.Highlight (ColourTheme(..), getColours)

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
          let colourTheme = detectColourTheme flags
          putStr $ prettify (getColours colourTheme) ast

      -- -- CODE GENERATION PHASE -- --

      -- compile AST into machine code, and output executable, if possible
      when (null flags || flagIsSet 'x' flags) $ do
          putStrLn "Sorry, can't generate code yet!"

detectColourTheme :: [Flag] -> ColourTheme
detectColourTheme flags
  | flagIsSet 'l' flags = LightTheme
  | flagIsSet 'd' flags = DarkTheme
  | otherwise           = NoTheme

-- ----------------------------------------------------------------------------
-- Processing command-line options
-- ----------------------------------------------------------------------------

-- The Opts structure will hold our options for this program: a list of
-- flags (Chars like 'p' for pretty-printing) and a single source file name:
data Opts
  = Opts [Flag] FilePath
type Flag
  = Char

-- flagIsSet
-- To interrogate the list of flags (basically `elem')
flagIsSet :: Flag -> [Flag] -> Bool
flagIsSet f flags
  = f `elem` flags

-- List of valid flags
validFlags
  = "xaphld"

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
      let invalidFlags = flags \\ validFlags
      unless (null invalidFlags) $ do
          errorExit $ "invalid flag(s): " ++ (intersperse ',' invalidFlags)
      let arguments = getArguments args
      case arguments of
        [sourceFileName] -> return (Opts flags sourceFileName)
        []               -> errorExit $ "missing required argument: file"
        (_:excess)       -> errorExit $ "excess positional argument(s): "
                                        ++ intercalate ", " excess

-- getFlags, getArguments
-- helper functions to extract flag characters and positional arguments
-- from the list of all args
getFlags  :: [String] -> [Flag]
getFlags args
  = nub $ concat [tail arg | arg <- args, head arg == '-']
getArguments :: [String] -> [String]
getArguments args
  = [arg | arg <- args, head arg /= '-']

-- helpExit
-- Display a message with usage information and then exit successfully.
helpExit :: IO a
helpExit
  = do
      printGoatHead
      printUsage
      exitSuccess

-- errorExit
-- Display an error message and then exit (with failure)
errorExit :: String -> IO a
errorExit problem
  = do
      putStrLn $ "Argument error:\n  " ++ problem
      printUsage
      exitWith (ExitFailure 1)

-- printUsage
-- Simply print a formatted usage message
printUsage :: IO ()
printUsage
   = do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " [-h] [-x] [-a] [-p[l|d]] [file]\n"
      putStrLn "Options and arguments:"
      putStrLn "  -x    : (or no flags) compile the file, print executable code"
      putStrLn "  -a    : parse the file and print the AST"
      putStrLn "  -p    : parse the file and pretty-print its source code"
      putStr $ "  -l    : " ++ dRainbow 0 "pretty-print WITH COLOUR! "
      putStrLn "(for light terminals)"
      putStr $ "  -d    : " ++ bRainbow 1 "pretty-print WITH COLOUR! "
      putStrLn "(for dark terminals)"
      putStrLn "  -h    : print this help message and exit"
      putStrLn "  file  : path to goat (.gt) file containing Goat source code"
      putStrLn "          (required argument for -x, -a and -p)"
      putStr "\n"

-- printGoatHead
-- Simply print a formatted goat head.
printGoatHead :: IO ()
printGoatHead
  = do
      putStrLn "==============================================================="
      putStrLn "       Goat: Compiler by PLI-DREAM-TEAM-TWENTEE-NINETEEN       "
      putStrLn "=========================*==*==*==*==*========================="
      putStrLn "                                          .--____."
      putStrLn "               ,_____._,                ,~-__,__,;\\"
      putStrLn "             ,/ /  / /~,\\              ~'-_ ,~_/__--\\"
      putStrLn "           ,~'\\/__:_; / ~\\,_          /-__ ~\\/  \\\";/,\\"
      putStrLn "          / \\ ,/\\_\\_~\\ /  /|,';;;;`,|\\  /\\/     \\=/- |"
      putStrLn "         ~--,/_/__  \\ ~\\ |  `._____.'  |/\\/     __---=--"
      putStrLn "        /==/./ \\ /\\  ;\\~\\_\\          _/\\/,,__--'._/-' `"
      putStrLn "       |==|/    \\==\\;  ;\\|  , \\ \\ / /L /::/ \\,~~ |==|-|"
      putStrLn "       |//\\\\,__/== |: ;  |L_\\  \\ V / /L::/-__/  /=/-,|"
      putStrLn "        \\ / | | \\ /: ;  ;\\ |\\\\  \\ / //|: |__\\_/=/--/,,"
      putStr $ "         \\______,/; ;   ;;\\ "++bGrn"@"++"|\\  | /|"++bCyn"@"
      putStrLn "|;: \\__\\__\\_/"
      putStrLn "                   ;   ;;;|\\/' \\ |/ '\\/;;"
      putStrLn "                  ;   ;;;;\\  {  \\|' }/;:'"
      putStrLn "                  ;  ;;;;;:| {   |  }|;'"
      putStrLn "                ;' ::::::;/    ./ \\  \\"
      putStrLn "                 `,'`'`';/   ./    \\  \\_"
      putStrLn "                    '''|____/   \\__/\\___|"
      putStrLn "                            \\_.  / _/"
      putStrLn "                      valkyrie \\/\\/"
      putStrLn "==============================================================="
-- goat from: http://www.ascii-art.de/ascii/ghi/goat.txt
-- note: above string is escaped; it will come out like this:
--                                    .--____.
--         ,_____._,                ,~-__,__,;\
--       ,/ /  / /~,\              ~'-_ ,~_/__--\
--     ,~'\/__:_; / ~\,_          /-__ ~\/  \";/,\
--    / \ ,/\_\_~\ /  /|,';;;;`,|\  /\/     \=/- |
--   ~--,/_/__  \ ~\ |  `._____.'  |/\/     __---=--
--  /==/./ \ /\  ;\~\_\          _/\/,,__--'._/-' `
-- |==|/    \==\;  ;\|  , \ \ / /L /::/ \,~~ |==|-|
-- |//\\,__/== |: ;  |L_\  \ V / /L::/-__/  /=/-,|
--  \ / | | \ /: ;  ;\ |\\  \ / //|: |__\_/=/--/,,
--   \______,/; ;   ;;\ @|\  | /|@|;: \__\__\_/
--             ;   ;;;|\/' \ |/ '\/;;
--            ;   ;;;;\  {  \|' }/;:'
--            ;  ;;;;;:| {   |  }|;'
--          ;' ::::::;/    ./ \  \
--           `,'`'`';/   ./    \  \_
--              '''|____/   \__/\___|
--                      \_.  / _/
--             valkyrie    \/\/

