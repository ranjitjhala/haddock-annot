module Language.Haskell.HsColour.Anchors
  ( insertAnchors
  ) where

import Language.Haskell.HsColour.Classify
import Language.Haskell.HsColour.General
import Data.List

-- This is an attempt to find the first defining occurrence of an
-- identifier (function, datatype, class) in a Haskell source file.
-- Rather than parse the module properly, we try to get by with just
-- a finite state automaton.  Keeping a record of identifiers we
-- have already seen, we look at the beginning of every line to see
-- if it starts with the right tokens to introduce a defn.  If so,
-- we look a little bit further until we can be certain.  Then plonk
-- (or not) an anchor at the beginning of the line.

type Anchor = String

-- | 'insertAnchors' places an anchor marker in the token stream before the
--   first defining occurrence of any identifier.  Here, /before/ means
--   immediately preceding its type signature, or preceding a (haddock)
--   comment that comes immediately before the type signature, or failing
--   either of those, before the first equation.
insertAnchors :: [(TokenType,String)] -> [Either Anchor (TokenType,String)]
insertAnchors = anchor emptyST

-- looks at first token in the left-most position of each line
-- precondition: have just seen a newline token.
anchor st s = case identifier st s of
                Nothing -> emit st s
                Just v  -> Left v: emit (insertST v st) s

-- emit passes stuff through until the next newline has been encountered,
-- then jumps back into the anchor function
-- pre-condition: newlines are explicitly single tokens
emit st (t@(Space,"\n"):stream) = Right t: anchor st stream
emit st (t:stream)              = Right t: emit st stream
emit _  []                      = []

-- Given that we are at the beginning of a line, determine whether there
-- is an identifier defined here, and if so, return it.
-- precondition: have just seen a newline token.
identifier st t@((kind,v):stream) | kind`elem`[Varid,Definition] =
    case skip stream of
        ((Varop,v):_) | not (v`inST`st) -> Just (fix v)
        notVarop  --  | typesig stream  -> Nothing    -- not a defn
                      | v `inST` st     -> Nothing    -- already defined
                      | otherwise       -> Just v
identifier st t@((Layout,"("):stream) =
    case stream of
      ((Varop,v):(Layout,")"):_)
                  --  | typesig stream  -> Nothing
	              | v `inST` st     -> Nothing
	              | otherwise	-> Just (fix v)
      notVarop -> case skip (munchParens stream) of
          ((Varop,v):_) | not (v`inST`st) -> Just (fix v)
          _             -> Nothing
identifier st t@((Keyword,"foreign"):stream) = Nothing -- not yet implemented
identifier st t@((Keyword,"data"):stream)    = getConid stream
identifier st t@((Keyword,"newtype"):stream) = getConid stream
identifier st t@((Keyword,"type"):stream)    = getConid stream
identifier st t@((Keyword,"class"):stream)   = getConid stream
identifier st t@((Comment,_):(Space,"\n"):stream) = identifier st stream
identifier st stream = Nothing

-- Is this really a type signature?  (no longer used)
typesig :: [(TokenType,String)] -> Bool
typesig ((Keyglyph,"::"):_)   = True
typesig ((Varid,_):stream)    = typesig stream
typesig ((Layout,"("):(Varop,_):(Layout,")"):stream)    = typesig stream
typesig ((Layout,","):stream) = typesig stream
typesig ((Space,_):stream)    = typesig stream
typesig ((Comment,_):stream)  = typesig stream
typesig _                     = False

-- throw away everything from opening paren to matching close
munchParens =  munch (0::Int)	-- already seen open paren
  where munch 0 ((Layout,")"):rest) = rest
        munch n ((Layout,")"):rest) = munch (n-1) rest
        munch n ((Layout,"("):rest) = munch (n+1) rest
        munch n (_:rest)            = munch n rest
        munch _ []                  = []	-- source is ill-formed

-- ensure anchor name is correct for a Varop
fix ('`':v) = dropLast '`' v
fix v       = v

-- look past whitespace and comments to next "real" token
skip ((Space,_):stream)   = skip stream
skip ((Comment,_):stream) = skip stream
skip stream               = stream

-- skip possible context up to and including "=>", returning next Conid token
-- (this function is highly partial - relies on source being parse-correct)
getConid stream =
    case skip stream of
        ((Conid,c):rest) -> case context rest of
                              ((Keyglyph,"="):_)     -> Just c
                              ((Keyglyph,"=>"):more) ->
                                  case skip more of
                                      ((Conid,c'):_) -> Just c'
                                      v -> debug v ("Conid "++c++" =>")
                              v -> debug v ("Conid "++c++" no = or =>")
        ((Layout,"("):rest) -> case context rest of
                                   ((Keyglyph,"=>"):more) ->
                                       case skip more of
                                           ((Conid,c'):_) -> Just c'
                                           v -> debug v ("(...) =>")
                                   v -> debug v ("(...) no =>")
        v -> debug v ("no Conid or (...)")
    where debug   _   _ = Nothing
       -- debug (s:t) c = error ("HsColour: getConid failed: "++show s
       --                       ++"\n  in the context of: "++c)

-- jump past possible class context
context stream@((Keyglyph,"="):_) = stream
context stream@((Keyglyph,"=>"):_) = stream
context (_:stream) = context stream
context [] = []

-- simple implementation of a string lookup table.
-- replace this with something more sophisticated if needed.
type ST = [String]

emptyST :: ST
emptyST = []

insertST :: String -> ST -> ST
insertST k st = insert k st

inST :: String -> ST -> Bool
inST k st = k `elem` st
