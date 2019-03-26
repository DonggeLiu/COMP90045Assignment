module Main where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
-- 
--                      GOAT - PARSER AND PRETTY-PRINTER
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

import System.Environment
import System.Exit

-- ----------------------------------------------------------------------------

-- TODO: build parser and pretty-printer

-- ----------------------------------------------------------------------------

main :: IO ()
main
  = do
      progName <- getProgName
      args <- getArgs
      case args of
        ["-p", sourceFile] -> putStrLn "Sorry, can't parse/pretty-print yet!"
        [sourceFile]       -> putStrLn "Sorry, can't generate code yet!"
        _                  -> do
                                putStr "Usage: "
                                putStrLn $ progName ++ " [-p] source_file"
                                exitWith $ ExitFailure 1
