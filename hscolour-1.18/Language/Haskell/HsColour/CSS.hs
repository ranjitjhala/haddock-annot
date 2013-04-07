-- | Formats Haskell source code as HTML with CSS.
module Language.Haskell.HsColour.CSS 
  ( hscolour
  , top'n'tail
  , renderToken 
  , pre 
  ) where

import Language.Haskell.HsColour.Anchors
import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.HTML (renderAnchors, renderComment,
                                       renderNewLinesAnchors, escape)

-- | Formats Haskell source code as a complete HTML document with CSS.
hscolour :: Bool   -- ^ Whether to include anchors.
         -> String -- ^ Haskell source code.
         -> String -- ^ An HTML document containing the coloured 
                   --   Haskell source code.
hscolour anchor = 
  pre
  . (if anchor 
        then renderNewLinesAnchors
             . concatMap (renderAnchors renderToken)
             . insertAnchors
        else concatMap renderToken)
  . tokenise

top'n'tail :: String -> String -> String
top'n'tail title  = (cssPrefix title ++) . (++cssSuffix)

pre :: String -> String
pre = ("<pre>"++) . (++"</pre>")

renderToken :: (TokenType, String) -> String
renderToken (cls,text) =
        before ++ (if cls == Comment then renderComment text else escape text) ++ after
    where
        before = if null cls2 then "" else "<span class='" ++ cls2 ++ "'>"
        after  = if null cls2 then "" else "</span>"
        cls2 = cssClass cls


cssClass Keyword  = "hs-keyword"
cssClass Keyglyph = "hs-keyglyph"
cssClass Layout   = "hs-layout"
cssClass Comment  = "hs-comment"
cssClass Conid    = "hs-conid"
cssClass Varid    = "hs-varid"
cssClass Conop    = "hs-conop"
cssClass Varop    = "hs-varop"
cssClass String   = "hs-str"
cssClass Char     = "hs-chr"
cssClass Number   = "hs-num"
cssClass Cpp      = "hs-cpp"
cssClass Error    = "hs-sel"
cssClass Definition = "hs-definition"
cssClass _        = ""


cssPrefix title = unlines
    ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    ,"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html>"
    ,"<head>"
    ,"<!-- Generated by HsColour, http://www.cs.york.ac.uk/fp/darcs/hscolour/ -->"
    ,"<title>"++title++"</title>"
    ,"<link type='text/css' rel='stylesheet' href='hscolour.css' />"
    ,"</head>"
    ,"<body>"
    ]
    
cssSuffix = unlines
    ["</body>"
    ,"</html>"
    ]
