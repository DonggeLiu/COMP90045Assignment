module Util.ColourParTTY where

import Data.List (cycle)

type Colour = String -> String

-- dark Colours
dBlk, dRed, dGrn, dYel, dBlu, dMgn, dCyn, dWht :: Colour
dBlk = esc 30
dRed = esc 31
dGrn = esc 32
dYel = esc 33
dBlu = esc 34
dMgn = esc 35
dCyn = esc 36
dWht = esc 37

-- bright Colours
bBlk, bRed, bGrn, bYel, bBlu, bMgn, bCyn, bWht :: Colour
bBlk = esc 90
bRed = esc 91
bGrn = esc 92
bYel = esc 93
bBlu = esc 94
bMgn = esc 95
bCyn = esc 96
bWht = esc 97


esc :: Int -> Colour
esc code string
  = "\ESC[" ++ show code ++ "m" ++ string ++ "\ESC[0m"

bColours :: [Colour]
bColours
  = [bRed, bYel, bGrn, bCyn, bBlu, bMgn]
dColours :: [Colour]
dColours
  = [dRed, dYel, dGrn, dCyn, dBlu, dMgn]

rainbow :: [Colour] -> Int -> String -> String
rainbow cols offset string
  = concat $ zipWith applyCol colourCycle string
    where
      applyCol col char = col (char:"")
      colourCycle = drop offset (cycle cols)

dRainbow
  = rainbow dColours

bRainbow
  = rainbow bColours