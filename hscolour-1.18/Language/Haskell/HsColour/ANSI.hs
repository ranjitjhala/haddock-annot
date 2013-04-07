-- | Partially taken from Hugs AnsiScreen.hs library:
module Language.Haskell.HsColour.ANSI
  ( highlightOn
  , highlightOff
  , highlight
  , cleareol, clearbol, clearline, clearDown, clearUp, cls
  , goto
  , cursorUp, cursorDown, cursorLeft, cursorRight
  , savePosition, restorePosition
  , Highlight(..)
  , Colour(..)
  , colourCycle
  , enableScrollRegion, scrollUp, scrollDown
  , lineWrap
  ) where

import Language.Haskell.HsColour.ColourHighlight

import Data.List (intersperse,isPrefixOf)
import Data.Char (isDigit)


-- Basic screen control codes:

type Pos           = (Int,Int)

at        :: Pos -> String -> String
-- | Move the screen cursor to the given position.
goto      :: Int -> Int -> String
home      :: String
-- | Clear the screen.
cls       :: String

at (x,y) s  = goto x y ++ s
goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
home        = goto 1 1

cursorUp    = "\ESC[A"
cursorDown  = "\ESC[B"
cursorRight = "\ESC[C"
cursorLeft  = "\ESC[D"

cleareol    = "\ESC[K"
clearbol    = "\ESC[1K"
clearline   = "\ESC[2K"
clearDown   = "\ESC[J"
clearUp     = "\ESC[1J"
-- Choose whichever of the following lines is suitable for your system:
cls         = "\ESC[2J"     -- for PC with ANSI.SYS
--cls         = "\^L"         -- for Sun window

savePosition    = "\ESC7"
restorePosition = "\ESC8"


-- data Colour    -- imported from ColourHighlight
-- data Highlight -- imported from ColourHighlight

instance Enum Highlight where
  fromEnum Normal       = 0
  fromEnum Bold         = 1
  fromEnum Dim          = 2
  fromEnum Underscore   = 4
  fromEnum Blink        = 5
  fromEnum ReverseVideo = 7
  fromEnum Concealed    = 8
  fromEnum (Foreground c) = 30 + fromEnum c
  fromEnum (Background c) = 40 + fromEnum c
  fromEnum Italic       = 2

-- | Make the given string appear with all of the listed highlights
highlight :: [Highlight] -> String -> String
highlight attrs s = highlightOn attrs ++ s ++ highlightOff

highlightOn []     = highlightOn [Normal]
highlightOn attrs  = "\ESC["
                     ++ concat (intersperse ";" (map (show.fromEnum) attrs))
                     ++"m"
highlightOff = "\ESC[0m"


-- | An infinite supply of colours.
colourCycle :: [Colour]
colourCycle = cycle [Red,Blue,Magenta,Green,Cyan]


-- | Scrolling
enableScrollRegion :: Int -> Int -> String
enableScrollRegion start end = "\ESC["++show start++';':show end++"r"

scrollDown  = "\ESCD"
scrollUp    = "\ESCM"

-- Line-wrapping mode
lineWrap True  = "\ESC[7h"
lineWrap False = "\ESC[7l"

