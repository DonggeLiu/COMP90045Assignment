module Util.ColourParTTY where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                You can't spell party without TTY- oh, wait...
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


-- ----------------------------------------------------------------------------
-- Making and using ANSI colour codes
-- ----------------------------------------------------------------------------

type Colour
  = String
type Colourer
  = Colour -> String

-- Basic colours
esc :: Int -> Colour
esc code
  = "\ESC[" ++ show code ++ "m"

setBlk1, setRed1, setGrn1, setYel1, setBlu1, setMgn1, setCyn1, setWht1 :: Colour
setBlk1 = esc 30
setRed1 = esc 31
setGrn1 = esc 32
setYel1 = esc 33
setBlu1 = esc 34
setMgn1 = esc 35
setCyn1 = esc 36
setWht1 = esc 37

setBlk2, setRed2, setGrn2, setYel2, setBlu2, setMgn2, setCyn2, setWht2 :: Colour
setBlk2 = esc 90
setRed2 = esc 91
setGrn2 = esc 92
setYel2 = esc 93
setBlu2 = esc 94
setMgn2 = esc 95
setCyn2 = esc 96
setWht2 = esc 97

-- Set the terminal back to default colours
reset :: Colour
reset   = esc 0


-- For truecolour terminals
escTrue :: (Int, Int, Int) -> Colour
escTrue (r, g, b)
  = "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

-- For 256-colour terminals
-- (n is in a 6x6x6 grid of colours from 16 to 231, or before 16 for basic
-- colours, or after 231 for greyscale colours)
esc256 :: Int -> Colour
esc256 n
  = "\ESC[38;5;" ++ show n ++ "m"

-- Cycle through truecolours
colourStepTrue :: Int -> (Int, Int, Int)
colourStepTrue i
  = (r, g, b)
  where
    rad = (fromIntegral i) / 180 * pi
    r = round $ 128 + 127 * (sin rad)
    g = round $ 128 + 127 * (sin $ rad - 2 * pi / 3)
    b = round $ 128 + 127 * (sin $ rad - 4 * pi / 3)

-- Cycle through more basic 8-bit colours instead
-- (Not all terminals support truecolour!)
colourStep256 :: Int -> Int
colourStep256 i
  = 16 + 36*r6 + 6*g6 + b6
  where
    (r256, g256, b256) = colourStepTrue i
    r6 = floor $ (fromIntegral r256 / 256) * 6
    g6 = floor $ (fromIntegral g256 / 256) * 6
    b6 = floor $ (fromIntegral b256 / 256) * 6


-- ----------------------------------------------------------------------------
-- Applying colouring to strings
-- ----------------------------------------------------------------------------

-- Wrap a string in a colour
withColour :: Colour -> Colourer
withColour colour string
  = colour ++ string ++ reset

-- dark Colours as functions
blk1, red1, grn1, yel1, blu1, mgn1, cyn1, wht1 :: Colourer
blk1 = withColour setBlk1
red1 = withColour setRed1
grn1 = withColour setGrn1
yel1 = withColour setYel1
blu1 = withColour setBlu1
mgn1 = withColour setMgn1
cyn1 = withColour setCyn1
wht1 = withColour setWht1

-- bright Colours as functions
blk2, red2, grn2, yel2, blu2, mgn2, cyn2, wht2 :: Colourer
blk2 = withColour setBlk2
red2 = withColour setRed2
grn2 = withColour setGrn2
yel2 = withColour setYel2
blu2 = withColour setBlu2
mgn2 = withColour setMgn2
cyn2 = withColour setCyn2
wht2 = withColour setWht2


-- Cycle those characters in a string matching a certain predicate
-- through colours, and 
rainbowMask :: (Char -> Bool) -> Int -> Int -> (String -> String)
rainbowMask mask speed offset string
  = concat $ zipWith wrapMaskChar colours string
  where
    colours
      = drop offset $ map (esc256 . colourStep256) [0,speed..]
    wrapMaskChar colour char
      | mask char = colour ++ char:"" ++ reset
      | otherwise = char:""


-- Default functions to cycle a string through various colours
rainbow :: Int -> Int -> String -> String
rainbow
  = rainbowMask (const True)


-- ----------------------------------------------------------------------------
-- Manipulating strings containing ANSI control codes.
-- ----------------------------------------------------------------------------

-- stripControlChars
-- Attempt to remove ANSI control codes from a string. Assumes only a single 
-- layer of control characters... not very robust!
stripControlChars :: String -> String
stripControlChars []
  = []
stripControlChars ('\ESC':restOfString)
  = stripControlChars $ tail $ dropWhile (/= 'm') $ restOfString
stripControlChars (c:cs)
  = c : stripControlChars cs

-- chunkControlChars
-- Split a string containing control sequences into chunks of a given length,
-- NOT counting the control sequences, but leaving them in the string.
chunkControlChars :: Int -> String -> [String]
chunkControlChars _ []
  = []
chunkControlChars n string
  = line1 : chunkControlChars n restOfString
  where 
    (line1, restOfString) = initDropControlChars n string

-- initDropControlChars
-- Split away a given number of characters from the start of a string, NOT 
-- counting control sequences, but leaving them in the strings.
initDropControlChars :: Int -> String -> (String, String)
initDropControlChars n ('\ESC':restOfString)
  = (escapeSequence ++ start, rest)
  where
    escapeSequence = '\ESC' : (takeWhile (/= 'm') restOfString) ++ "m"
    afterEscapeSeq = tail $ dropWhile (/= 'm') restOfString
    (start, rest)  = initDropControlChars n afterEscapeSeq
initDropControlChars 0 s
  = ("", s)
initDropControlChars n []
  = ("", "")
initDropControlChars n (c:cs)
  = (c : start, rest)
  where
     (start, rest) = initDropControlChars (n-1) cs
