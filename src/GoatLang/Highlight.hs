module GoatLang.Highlight where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                     UTILITY - MONADIC STRING BUILDER
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


import Util.ColourParTTY

data ColourSchemeName = NoColours | LightColours | DarkColours

data ColourScheme
  = ColourScheme { keyword :: Colour
                 , string  :: Colour
                 , literal :: Colour
                 , ident   :: Colour
                 }

getColours :: ColourSchemeName -> ColourScheme
getColours NoColours
  = ColourScheme id id id id
getColours LightColours
  = ColourScheme { keyword = dMgn
                 , string  = dGrn
                 , literal = dBlu
                 , ident   = dCyn
                 }
getColours DarkColours
  = ColourScheme { keyword = bMgn
                 , string  = bGrn
                 , literal = bYel
                 , ident   = bCyn
                 }

