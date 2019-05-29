module Util.ColourParTTY where

import Data.List (cycle)

type Colour
  = String
type Colourer
  = Colour -> String


esc :: Int -> Colour
esc code
  = "\ESC[" ++ show code ++ "m"

esc256 :: Int -> Colour
esc256 code
  = "\ESC[38;5;" ++ show code ++ "m"

escTrue :: (Int, Int, Int) -> Colour
escTrue (r, g, b)
  = "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

reset :: Colour
reset   = esc 0

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

with :: Colour -> Colourer
with colour string
  = colour ++ string ++ reset

-- dark Colours
blk1, red1, grn1, yel1, blu1, mgn1, cyn1, wht1 :: Colourer
blk1 = with setBlk1
red1 = with setRed1
grn1 = with setGrn1
yel1 = with setYel1
blu1 = with setBlu1
mgn1 = with setMgn1
cyn1 = with setCyn1
wht1 = with setWht1

-- bright Colours
blk2, red2, grn2, yel2, blu2, mgn2, cyn2, wht2 :: Colourer
blk2 = with setBlk2
red2 = with setRed2
grn2 = with setGrn2
yel2 = with setYel2
blu2 = with setBlu2
mgn2 = with setMgn2
cyn2 = with setCyn2
wht2 = with setWht2


colours1 :: [Colour]
colours1
  = [setRed1, setYel1, setGrn1, setCyn1, setBlu1, setMgn1]

colours2 :: [Colour]
colours2
  = [setRed2, setYel2, setGrn2, setCyn2, setBlu2, setMgn2]

rainbow :: [Colour] -> String -> String
rainbow cols string
  = interleave (cycle cols) string
    where
      interleave _ [] = reset
      interleave (c:cs) (s:ss) = c ++ s : interleave cs ss

rainbow1
  = rainbow colours1

rainbow2
  = rainbow (drop 1 colours2) -- offset is nice?

-- stripControlChars
-- NOTE: Assumes only a single layer of control characters... not very robust!
-- E.g.
--   λ rainbow1 "hello, world!"
--   "\ESC[31mh\ESC[33me\ESC[32ml\ESC[36ml\ESC[34mo\ESC[35m,\ESC[31m 
--   \ESC[33mw\ESC[32mo\ESC[36mr\ESC[34ml\ESC[35md\ESC[31m!\ESC[0m"
--   λ stripControlChars $ rainbow1 "hello, world!"
--   "hello, world!"
stripControlChars :: String -> String
stripControlChars []
  = []
stripControlChars ('\ESC':restOfString)
  = stripControlChars $ tail $ dropWhile (/= 'm') $ restOfString
stripControlChars (c:cs)
  = c:stripControlChars cs

rainbowMask :: (Char -> Bool) -> Int -> Int -> (String -> String)
rainbowMask mask speed offset string
  = concat $ zipWith wrapColour colours string
  where
    colours
      = drop offset $ map (esc256 . colourStep256) [0,speed..]
    wrapColour colour char
      | mask char = colour ++ char:"" ++ reset
      | otherwise = char:""

colourStepTrue :: Int -> (Int, Int, Int)
colourStepTrue i
  = (r, g, b)
  where
    rad = (fromIntegral i) / 180 * pi
    r = round $ 128 + 127 * (sin rad)
    g = round $ 128 + 127 * (sin $ rad + 2 * pi / 3)
    b = round $ 128 + 127 * (sin $ rad + 4 * pi / 3)

colourStep256 :: Int -> Int
colourStep256 i
  = 16 + 36*r6 + 6*g6 + b6
  where
    (r256, g256, b256) = colourStepTrue i
    r6 = floor $ (fromIntegral r256 / 256) * 6
    g6 = floor $ (fromIntegral g256 / 256) * 6
    b6 = floor $ (fromIntegral b256 / 256) * 6


-- color = sum $ 16 : [ int(6 * float(val) / 256) * mod
--   | (val, mod) <- zip(rgb, [36, 6, 1])])