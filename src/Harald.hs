module Main where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--    "It is hard to think of better examples of strong synergy between
--     theory and practice than the development of programming language
--     technologies."
--                                                -- Harald Søndergaard
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

import Data.List.Split (chunksOf)

import Util.ColourParTTY (stripControlChars, rainbowMask)

-- ----------------------------------------------------------------------------
-- Output configuration
-- ----------------------------------------------------------------------------

width = 70
point = 42
colourSpeed = 3

-- ----------------------------------------------------------------------------
-- Program entry-point
-- ----------------------------------------------------------------------------

main :: IO ()
main
  = do
      speechBubbleTop
      contents <- getContents
      mapM speechBubbleLongLine $ lines contents
      speechBubbleBot
      rainbow harald


-- ----------------------------------------------------------------------------
-- Constructing the speech bubble
-- ----------------------------------------------------------------------------

speechBubbleTop :: IO ()
speechBubbleTop
  = do
      putChar '/'
      putStr $ replicate width '`'
      putChar '\\'
      putChar '\n'

speechBubbleBot :: IO ()
speechBubbleBot
  = do
      putChar '\\'
      putStr $ replicate point '_'
      putStr $ replicate 5  ' '
      putStr $ replicate (width - 5 - point) '_'
      putChar '/'
      putChar '\n'

speechBubbleLongLine :: String -> IO ()
speechBubbleLongLine line
  = mapM_ speechBubbleSingleLine $ chunksOf (width-1) line
      
speechBubbleSingleLine :: String -> IO ()
speechBubbleSingleLine line
  = do
      let len = length $ stripControlChars line
      putChar '|'
      putChar ' '
      putStr line
      putStr $ replicate (width - 1 - len) ' '
      putChar '|'
      putChar '\n'


-- ----------------------------------------------------------------------------
-- Ascii art
-- ----------------------------------------------------------------------------

rainbow :: [String] -> IO ()
rainbow speakerLines
  = do
      -- Get a function that will turn a line's # symbols into a rainbow:
      let colourer = rainbowMask (== '#') colourSpeed
      -- Run it over the lines of the speaker's ASCII art:
      let colouredLines = zipWith colourer [0,colourSpeed..] speakerLines
      -- Put those lines on the screen!
      putStr $ unlines colouredLines

-- harald
-- Our favourite lecturer
harald :: [String]
harald
  = [ "#################`.------...`##############|   /########################"
    , "###############`.--...`````.-..-`##########|  /#########################"
    , "#############`:so-.``       ``./ys-########| /##########################"
    , "############-yd+..``         ``-:oh/#######|/###########################"
    , "###########:hh:-.``          ``.-:/o:.##################################"
    , "##########:sh:..```          ``..-//++.#################################"
    , "##########oy+--.``            ```-+++sy#################################"
    , "##########oh/:-..```````   ``--..:oosyh#################################"
    , "#########`ds/:---.--/++/. `/sso//::+oos#################################"
    , "##########oo/:---///+//..``o/.....`:oo-#################################"
    , "##########`:/::.````````.-`++.``  `-o/-#################################"
    , "###########..::-``    `.--`.so-` `.+s+-#################################"
    , "###########``//-..```:-/yo+ymNhy/:+sso:#################################"
    , "###########`-++:-../o:..--+oyhyhhooss+.#################################"
    , "############./+:-..+:+so+++ohmmh:.//s.##################################"
    , "##############/+:....-:+//++os+...oo+###################################"
    , "##############.oo:--.``.-:/+//:.:sho`###################################"
    , "##############:o/oo//.`````..../hmh`####################################"
    , "#############:h+/:oyss/-.---:+ymNm+#####################################"
    , "###########`/oyo://ossydmmmmNMMNmds`####################################"
    , "#########`-+sssy+--/+yhdNNMMMMNmddMh`###################################"
    , "#####`.:oyosssyyys-.-/ohdmNNMNmdddMNy.##################################"
    , "##`:+osyhdsyssyysyys/:/+osyyhdddhmNmdhh+`###############################"
    ]