module Main where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                                  BAAAAAAAA
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

import Util.ColourParTTY (stripControlChars, rainbowMask)

-- ----------------------------------------------------------------------------
-- Output settings
-- ----------------------------------------------------------------------------

width = 63
colourSpeed = 3

-- ----------------------------------------------------------------------------
-- Program entry-point
-- ----------------------------------------------------------------------------

main :: IO ()
main
  = do
      speechBubbleTop
      contents <- getContents
      mapM speechBubbleLine $ lines contents
      speechBubbleBot
      speaking goatHead


-- ----------------------------------------------------------------------------
-- Constructing the speech bubble
-- ----------------------------------------------------------------------------

speechBubbleTop :: IO ()
speechBubbleTop
  = do
      putChar '/'
      putStr $ replicate (width-2) '`'
      putChar '\\'
      putChar '\n'

speechBubbleBot :: IO ()
speechBubbleBot
  = do
      putChar '\\'
      putStr $ replicate (width-2) '_'
      putChar '/'
      putChar '\n'

speechBubbleLine :: String -> IO ()
speechBubbleLine line
  = do
      let len = length $ stripControlChars line
      putChar '|'
      putChar ' '
      putStr line
      putStr $ replicate (width - 3 - len) ' '
      putChar '|'
      putChar '\n'

speaking :: [String] -> IO ()
speaking speakerLines
  = do
      let colourer = rainbowMask (== '#') colourSpeed
      let colouredLines = zipWith colourer [0,2*colourSpeed..] speakerLines
      putStr $ unlines colouredLines

-- ----------------------------------------------------------------------------
-- Ascii art
-- ----------------------------------------------------------------------------

-- goat from: http://www.ascii-art.de/ascii/ghi/goat.txt
goatHead :: [String]
goatHead
  = [ "###############################################################"
    , "###############################################################"
    , "###############################################################"
    , "#########################################.--____.##############"
    , "##############,_____._,################,~-__,__,;\\#############"
    , "############,/ /  / /~,\\##############~'-_ ,~_/__--\\###########"
    , "##########,~'\\/__:_; / ~\\,_##########/-__ ~\\/  \\\";/,\\##########"
    , "#########/ \\ ,/\\_\\_~\\ /  /|,';;;;`,|\\  /\\/     \\=/- |##########"
    , "########~--,/_/__  \\ ~\\ |  `._____.'  |/\\/     __---=--########"
    , "#######/==/./ \\ /\\  ;\\~\\_\\          _/\\/,,__--'._/-' `#########"
    , "######|==|/    \\==\\;  ;\\|  , \\ \\ / /L /::/ \\,~~ |==|-|#########"
    , "######|//\\\\,__/== |: ;  |L_\\  \\ V / /L::/-__/  /=/-,|##########"
    , "#######\\ / | | \\ /: ;  ;\\ |\\\\  \\ / //|: |__\\_/=/--/,,##########"
    , "########\\______,/; ;   ;;\\ @|\\  | /|@|;: \\__\\__\\_/#############"
    , "##################;   ;;;|\\/' \\ |/ '\\/;;#######################"
    , "#################;   ;;;;\\  {  \\|' }/;:'#######################"
    , "#################;  ;;;;;:| {   |  }|;'########################"
    , "###############;' ::::::;/    ./ \\  \\##########################"
    , "################`,'`'`';/   ./    \\  \\_########################"
    , "###################'''|____/   \\__/\\___|#######################"
    , "###########################\\_.  / _/###########################"
    , "##################valkyrie####\\/\\/#############################"
    , "###############################################################"
    , "###############################################################"
    , "###############################################################"
    ]
