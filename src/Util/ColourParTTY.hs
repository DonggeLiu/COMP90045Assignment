module Util.ColourParTTY where

import Data.List (cycle)

type Colour
  = String
type Colourer
  = Colour -> String


esc :: Int -> Colour
esc code
  = "\ESC[" ++ show code ++ "m"
-- esc5 code
--   = "\ESC[5;" ++ show code ++ "m"

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