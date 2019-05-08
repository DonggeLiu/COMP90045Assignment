module GoatLang.Highlight where

import Util.ColourParTTY

data ColourTheme = NoTheme | LightTheme | DarkTheme

data Colours
  = Colours { keyword :: Colour
            , string  :: Colour
            , literal :: Colour
            , ident   :: Colour
            }

getColours :: ColourTheme -> Colours
getColours NoTheme
  = Colours id id id id
getColours LightTheme
  = Colours { keyword = dMgn
            , string  = dGrn
            , literal = dBlu
            , ident   = dCyn
            }
getColours DarkTheme
  = Colours { keyword = bMgn
            , string  = bGrn
            , literal = bYel
            , ident   = bCyn
            }