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

import System.Environment

import Util.ColourParTTY (stripControlChars, rainbowMask, chunkControlChars)


-- ----------------------------------------------------------------------------
-- Output configuration
-- ----------------------------------------------------------------------------

width = 70      -- maximum output line width (long lines are wrapped)
point = 42      -- the answer to life, the universe, and everything
colourSpeed = 3 -- how much rainbow can you handle?


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

      args <- getArgs
      case args of
        ["-l"] -> rainbow haraldForLightTerminal
        ["-d"] -> rainbow haraldForDarkTerminal
        _ -> do
          rainbow haraldForDarkTerminal
          putStrLn "WARNING:"
          putStrLn "Your viewing experience may not be optimal if you are not"
          putStrLn "using a dark terminal. Please specify -d for dark terminals"
          putStrLn "or -l for light terminal to silence this warning. Thanks."


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
  = mapM_ speechBubbleSingleLine $ chunkControlChars (width-1) line
      
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

-- rainbow
-- Print an ascii-art list of lines with a 256 colour rainbow
rainbow :: [String] -> IO ()
rainbow speakerLines
  = do
      -- Get a function that will turn a line's # symbols into a rainbow:
      let colourer = rainbowMask (== '#') colourSpeed
      -- Run it over the lines of the speaker's ASCII art:
      let colouredLines = zipWith colourer [0,colourSpeed..] speakerLines
      -- Put those lines on the screen!
      putStr $ unlines colouredLines

-- haraldForLightTerminal, haraldForDarkTerminal
-- A dedication to our favourite lecturer
haraldForLightTerminal :: [String]
haraldForLightTerminal
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
haraldForDarkTerminal :: [String]
haraldForDarkTerminal
  = [ "###########################################|   /########################"
    , "#################mdhhhhhhddd###############|  /#########################"
    , "################dhhdddmmmmmdhddd###########| /##########################"
    , "##############h+ohddmNMMMMNNmdhs/+d########|/###########################"
    , "############h+:shddmNNNNNMNNNNdhyo/y####################################"
    , "###########y//yhddmNMMMNMMNNNmmhhysoym##################################"
    , "##########h+/yhhmmmNNMMNNNNNNmdddhsyosd#################################"
    , "##########o/ohhhmmNNNMNNNNNNNNmmdhoss++#################################"
    , "##########o/syhhdmmmmmmNNNNmmhdddyooo+/#################################"
    , "##########:+syhyhhhhysoydNms++osyyysooo#################################"
    , "##########oosyhhhyyysyyddmmoyddmmdmyooh#################################"
    , "##########mysyyhmmmmmmmddhmssdmNNNmhosd#################################"
    , "###########dhyyhmmNNNNmhhhmd+ohmNmds+sd#################################"
    , "###########mmysyddmNmyhs/oo+../+yys++oh#################################"
    , "###########mhssyhhhyoydhhhs+/////oo++sm#################################"
    , "############mysyhhdsyo+ossoo/-.:ydyy+m##################################"
    , "##############ysyddddhyssysso+sdddoos###################################"
    , "##############mooyhhdmmdhyysssydy+/s####################################"
    , "##############yosoossdmmmmmddddy/-/#####################################"
    , "#############h:ssyo++oyhdhhhhs/-.-s#####################################"
    , "############yo/oysso++/:-...```.-:+#####################################"
    , "#########mds+++/shhyo+/:-.`  `.-::`/####################################"
    , "#####mdho+o++++++ohdhso/:--...-:/:`./m##################################"
    , "####so++::+++++++//+sysoo++//////..-://s################################"
    ]