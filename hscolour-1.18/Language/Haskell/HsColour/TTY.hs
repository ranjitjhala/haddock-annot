-- | Highlights Haskell code with ANSI terminal codes.
module Language.Haskell.HsColour.TTY (hscolour) where

import Language.Haskell.HsColour.ANSI as ANSI
import Language.Haskell.HsColour.Classify
import Language.Haskell.HsColour.Colourise

-- | Highlights Haskell code with ANSI terminal codes.
hscolour :: ColourPrefs -- ^ Colour preferences.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour pref = concatMap (renderToken pref) . tokenise

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (t,s) = ANSI.highlight (colourise pref t) s
