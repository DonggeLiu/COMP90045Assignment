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

width = 78      -- maximum output line width (long lines are wrapped)
point = 42      -- the answer to life, the universe, and everything
colourSpeed = 3 -- how much rainbow can you handle?


-- ----------------------------------------------------------------------------
-- Program entry-point
-- ----------------------------------------------------------------------------

main :: IO ()
main
  = do
      speechBubbleTop
      contents <- fmap preprocess getContents
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
    where
      -- remove pesky carriage returns
      preprocess ('\r':cs)
        = preprocess cs
      -- expand tab characters to four spaces
      preprocess ('\t':cs)
        = ' ':' ':' ':' ':preprocess cs
      -- the rest of the input remains
      preprocess (c:cs)
        = c:preprocess cs
      preprocess []
        = []

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
  = [ "#################`.------...`##############|   /##" ++ replicate 30 '#'
    , "###############`.--...`````.-..-`##########|  /###" ++ replicate 30 '#'
    , "#############`:so-.``       ``./ys-########| /####" ++ replicate 30 '#'
    , "############-yd+..``         ``-:oh/#######|/#####" ++ replicate 30 '#'
    , "###########:hh:-.``          ``.-:/o:.############" ++ replicate 30 '#'
    , "##########:sh:..```          ``..-//++.###########" ++ replicate 30 '#'
    , "##########oy+--.``            ```-+++sy###########" ++ replicate 30 '#'
    , "##########oh/:-..```````   ``--..:oosyh###########" ++ replicate 30 '#'
    , "#########`ds/:---.--/++/. `/sso//::+oos###########" ++ replicate 30 '#'
    , "##########oo/:---///+//..``o/.....`:oo-###########" ++ replicate 30 '#'
    , "##########`:/::.````````.-`++.``  `-o/-###########" ++ replicate 30 '#'
    , "###########..::-``    `.--`.so-` `.+s+-###########" ++ replicate 30 '#'
    , "###########``//-..```:-/yo+ymNhy/:+sso:###########" ++ replicate 30 '#'
    , "###########`-++:-../o:..--+oyhyhhooss+.###########" ++ replicate 30 '#'
    , "############./+:-..+:+so+++ohmmh:.//s.############" ++ replicate 30 '#'
    , "##############/+:....-:+//++os+...oo+#############" ++ replicate 30 '#'
    , "##############.oo:--.``.-:/+//:.:sho`#############" ++ replicate 30 '#'
    , "##############:o/oo//.`````..../hmh`##############" ++ replicate 30 '#'
    , "#############:h+/:oyss/-.---:+ymNm+###############" ++ replicate 30 '#'
    , "###########`/oyo://ossydmmmmNMMNmds`##############" ++ replicate 30 '#'
    , "#########`-+sssy+--/+yhdNNMMMMNmddMh`#############" ++ replicate 30 '#'
    , "#####`.:oyosssyyys-.-/ohdmNNMNmdddMNy.############" ++ replicate 30 '#'
    , "##`:+osyhdsyssyysyys/:/+osyyhdddhmNmdhh+`#########" ++ replicate 30 '#'
    ]
haraldForDarkTerminal :: [String]
haraldForDarkTerminal
  = [ "###########################################|   /##" ++ replicate 30 '#'
    , "#################mdhhhhhhddd###############|  /###" ++ replicate 30 '#'
    , "################dhhdddmmmmmdhddd###########| /####" ++ replicate 30 '#'
    , "##############h+ohddmNMMMMNNmdhs/+d########|/#####" ++ replicate 30 '#'
    , "############h+:shddmNNNNNMNNNNdhyo/y##############" ++ replicate 30 '#'
    , "###########y//yhddmNMMMNMMNNNmmhhysoym############" ++ replicate 30 '#'
    , "##########h+/yhhmmmNNMMNNNNNNmdddhsyosd###########" ++ replicate 30 '#'
    , "##########o/ohhhmmNNNMNNNNNNNNmmdhoss++###########" ++ replicate 30 '#'
    , "##########o/syhhdmmmmmmNNNNmmhdddyooo+/###########" ++ replicate 30 '#'
    , "##########:+syhyhhhhysoydNms++osyyysooo###########" ++ replicate 30 '#'
    , "##########oosyhhhyyysyyddmmoyddmmdmyooh###########" ++ replicate 30 '#'
    , "##########mysyyhmmmmmmmddhmssdmNNNmhosd###########" ++ replicate 30 '#'
    , "###########dhyyhmmNNNNmhhhmd+ohmNmds+sd###########" ++ replicate 30 '#'
    , "###########mmysyddmNmyhs/oo+../+yys++oh###########" ++ replicate 30 '#'
    , "###########mhssyhhhyoydhhhs+/////oo++sm###########" ++ replicate 30 '#'
    , "############mysyhhdsyo+ossoo/-.:ydyy+m############" ++ replicate 30 '#'
    , "##############ysyddddhyssysso+sdddoos#############" ++ replicate 30 '#'
    , "##############mooyhhdmmdhyysssydy+/s##############" ++ replicate 30 '#'
    , "##############yosoossdmmmmmddddy/-/###############" ++ replicate 30 '#'
    , "#############h:ssyo++oyhdhhhhs/-.-s###############" ++ replicate 30 '#'
    , "############yo/oysso++/:-...```.-:+###############" ++ replicate 30 '#'
    , "#########mds+++/shhyo+/:-.`  `.-::`/##############" ++ replicate 30 '#'
    , "#####mdho+o++++++ohdhso/:--...-:/:`./m############" ++ replicate 30 '#'
    , "####so++::+++++++//+sysoo++//////..-://s##########" ++ replicate 30 '#'
    ]