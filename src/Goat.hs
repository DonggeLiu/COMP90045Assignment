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


import Text.Parsec
import Text.Parsec.Error

import Util.ColourParTTY
import Util.CodeWriter (ColourSchemeName(..),ColourScheme,getColourSchemeByName)


import GoatLang.Parser (parseProgram)
import GoatLang.PrettyPrint (printGoatProgramColoured)
import GoatLang.CodeGen (genCode)
import GoatLang.OzCode (printOzProgramColoured)

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
        Left err  -> syntaxErrorExit sourceCode err
        Right ast -> return ast

      -- handle the parsed program:
      when (flagIsSet 'a' flags) $ do
          print ast
      when (flagIsSet 'p' flags) $ do
          printGoatProgramColoured (detectColourScheme flags) ast

      -- -- CODE GENERATION PHASE -- --

      -- compile AST into machine code, and output executable, if possible
      -- when (null flags || flagIsSet 'x' flags) $ do
      --     let code = genCode ast
      --     printOzProgramColoured (detectColourScheme flags) code


detectColourScheme :: [Flag] -> ColourScheme
detectColourScheme flags
  | flagIsSet 'l' flags = getColourSchemeByName LightTerminal
  | flagIsSet 'd' flags = getColourSchemeByName DarkTerminal
  | otherwise           = getColourSchemeByName NoColours

-- ----------------------------------------------------------------------------
-- Handling errors in the source program
-- ----------------------------------------------------------------------------

-- syntaxErrorExit
-- Display syntax error diagnostic information to the user, including the
-- relevant section of the source program, then exit (with failure).
syntaxErrorExit :: String -> ParseError -> IO a
syntaxErrorExit src err
  = do
      let pos = errorPos err
      let lineNum = sourceLine pos
      let colNum = sourceColumn pos

      putStrLn $ red1 "Syntax error" ++ " at " ++ show pos ++ ":"
      -- show 3 lines of context:
      putStr   $ unlines $ take (min lineNum 3) $ drop (lineNum-3) $ lines src
      -- point to the problem:
      -- (and surrounding characters, since sometimes parsec point off-by-1)
      putStr   $ take (colNum-2) (repeat ' ') ++ red1 "^^^"
      putStrLn $ showErrorMessagesDefaults (errorMessages err)

      exitWith (ExitFailure 2)

-- showErrorMessagesDefaults
-- wrap Parsec's showErrorMessages function, which requires a bunch of template
-- strings be provided.
showErrorMessagesDefaults :: [Message] -> String
showErrorMessagesDefaults
  = showErrorMessages
      "or" "unknown parse error" "expecting" "unexpected" "end of input"

-- TODO: Something similar for semantic errors


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
          argsErrorExit $ "invalid flag(s): " ++ (intersperse ',' invalidFlags)
      let arguments = getArguments args
      case arguments of
        [sourceFileName] -> return (Opts flags sourceFileName)
        []               -> argsErrorExit $ "missing required argument: file"
        (_:excess)       -> argsErrorExit $ "excess positional argument(s): "
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

-- argsErrorExit
-- Display an error message and then exit (with failure)
argsErrorExit :: String -> IO a
argsErrorExit problem
  = do
      putStrLn $ red1 "Argument error" ++ ":\n  " ++ problem
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
      putStr $ "  -l    : " ++ rainbow1 "pretty-print WITH COLOUR! "
      putStrLn "(for light terminals)"
      putStr $ "  -d    : " ++ rainbow2 "pretty-print WITH COLOUR! "
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
      putStr $ "         \\______,/; ;   ;;\\ "++grn2"@"++"|\\  | /|"++cyn2"@"
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
