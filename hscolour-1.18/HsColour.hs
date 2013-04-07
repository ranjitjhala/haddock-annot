{-# LANGUAGE CPP #-}
module Main where

import Language.Haskell.HsColour
import qualified Language.Haskell.HsColour as HSColour
import Language.Haskell.HsColour.Colourise (readColourPrefs)
import Language.Haskell.HsColour.Options
import Language.Haskell.HsColour.ACSS (breakS, srcModuleName)

import System.Environment
import System.Exit
import System.IO hiding (withFile)
import Control.Monad (liftM, liftM2, when, forM_)
import Control.Exception (bracket)
import Data.List  (intersperse, isSuffixOf)
--import Debug.Trace

-- Deal with UTF-8 I/O.
#if __GLASGOW_HASKELL__ > 611
-- possibly if MIN_VERSION_base(4,2,0)
import System.IO (hSetEncoding, utf8)
#endif

version = "1.18"

optionTable :: [(String,Option)]
optionTable = [ ("help",    Help)
              , ("version", Version)
              , ("print-css", Information)
              , ("html",   Format HTML)
              , ("css",    Format CSS)
              , ("icss",   Format ICSS)
              , ("tty",    Format TTY)
              , ("latex",  Format LaTeX)
              , ("mirc",   Format MIRC)
              , ("lit",    LHS True)
              , ("lit-tex",LHS True)
              , ("nolit",  LHS False)
              , ("anchor",    Anchors True)
              , ("noanchor",  Anchors False)
              , ("partial",   Partial True)
              , ("nopartial", Partial False)
              ]

parseOption :: String -> Either String Option
parseOption ('-':'o':s) 
  = Right (Output s)
parseOption ('-':'a':'c':'s':'s':'=':s) 
  = Right (Annot s)
parseOption s@('-':_)
  = maybe (Left s) Right (lookup (dropWhile (== '-') s) optionTable)
parseOption s 
  = Right (Input s)

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  pref <- readColourPrefs
  let options = map parseOption args
      bad     = [ o | Left o <- options ]
      good    = [ o | Right o <- options ]
      formats = [ f | Format f <- good ] ++ [ ACSS | Annot _ <- good ]
      outFile = [ f | Output f <- good ]
      annFile = [ f | Annot f <- good ]
      output    = useDefault  TTY         id           formats
      anchors   = useDefault  False       id           [ b | Anchors b <- good ]
      partial   = useDefault  False       id           [ b | Partial b <- good ]
      lhs       = useDefault  Nothing     id           [ Just b | LHS b<- good ]
      title     = useDefault  "Haskell code" id        [ f | Input f   <- good ]
      ioWrapper = useDefaults (ttyInteract  outFile (guessLiterate lhs ""))
                              (fileInteract outFile annFile) 
                              [(f, guessLiterate lhs f) | Input f  <- good ]
  when (not (null bad)) $
       errorOut ("Unrecognised option(s): "++unwords bad++"\n"++usage prog)
  when (Help `elem` good)        $ writeResult [] (usage prog)
  when (Version `elem` good)     $ writeResult [] (prog++" "++version)
  when (Information `elem` good) $ writeResult outFile cssDefaults
  when (length formats > 1) $
       errorOut ("Can only choose one output format at a time: "
                 ++unwords (map show formats))
  when (length outFile > 1) $
       errorOut ("Can only have one output file at a time.")
  when (length annFile > 1) $
       errorOut ("Can only use a single annotation file for annotated-CSS output")
  
  ioWrapper (HSColour.hscolour output pref anchors partial title)

  where
    writeResult outF s = do if null outF then putStr s
                                         else writeUTF8File (last outF) s
                            exitSuccess
    fileInteract out ann inFs u 
      = do h <- case out of
                  []     -> return stdout
                  [outF] -> openFile outF WriteMode >>= set_utf8_io_enc
           forM_ inFs $ \ (f, lit) -> do
             src <- readUTF8File f
             a   <- readAnnots src ann
             hPutStr h $ u lit $ src ++ a 
           hClose h
    ttyInteract []     lit u = do hSetBuffering stdout NoBuffering
                                  Prelude.interact (u lit)
    ttyInteract [outF] lit u = do c <- hGetContents stdin
                                  writeUTF8File outF (u lit c)
    exitSuccess = exitWith ExitSuccess
    errorOut s  = hPutStrLn stderr s >> hFlush stderr >> exitFailure
    usage prog  = "Usage: "++prog
                  ++" options [file.hs]\n    where\n      options = [ "
                  ++ (indent 15 . unwords . width 58 58 . intersperse "|"
                     . ("-oOUTPUT":)
                     . ("-a=ANNOTFILE":)
                     . map (('-':) . fst)) optionTable
                  ++ " ]\n"
    useDefault d f list  | null list = d
                         | otherwise = f (head list)
    useDefaults d f list | null list = d
                         | otherwise = f list
    guessLiterate Nothing  f = ".lhs" `isSuffixOf` f || ".ly" `isSuffixOf` f
                               || ".lx" `isSuffixOf` f
    guessLiterate (Just b) _ = b
    readAnnots _   []     = return ""
    readAnnots src [annF] = do putStrLn $ "HsColour Annot on Module: " ++ mname
                               annots <- readUTF8File annF
                               return $ breakS ++ "\n" ++ mname ++ "\n" ++ annots
                            where mname = srcModuleName src

-- some simple text formatting for usage messages
width n left  []    = []
width n left (s:ss) = if size > left then "\n":s : width n n             ss
                                     else      s : width n (left-size-1) ss
  where size = length s

indent n [] = []
indent n ('\n':s) = '\n':replicate n ' '++indent n s
indent n (c:s)    = c: indent n s

-- Rather than have a separate .css file, define some reasonable defaults here.
cssDefaults = concat
  [ ".hs-keyglyph, .hs-layout {color: red;}\n"
  , ".hs-keyword {color: blue;}\n"
  , ".hs-comment, .hs-comment a {color: green;}\n"
  , ".hs-str, .hs-chr {color: teal;}\n"
  , ".hs-keyword, .hs-conid, .hs-varid, .hs-conop, .hs-varop, .hs-num, "
  , ".hs-cpp, .hs-sel, .hs-definition {}\n"
  ]

-- Deal with UTF-8 input and output.
set_utf8_io_enc :: Handle -> IO Handle
#if __GLASGOW_HASKELL__ > 611
-- possibly if MIN_VERSION_base(4,2,0)
set_utf8_io_enc h = do hSetEncoding h utf8; return h
#else
set_utf8_io_enc h = return h
#endif

-- FILE I(unput) / O(utput) is in UTF8
-- TTY  I(unput) / O(utput) is in locale
--   ( may have problems with HsColour IFILE >OFILE
--   , as it differs from HsColour IFILE -oOFILE)
-- TTY stderr is always in locale (always used for user interaction)
--
-- Some common use cases:
-- File I / FILE O (HsColour -css -anchor -oOFILE IFILE)
--                 : are both always done in UTF8 mode (cabal hscolour mode)
-- File I / TTY  O (HsColour IFILE)
--                 : file is read in UTF-8 written in locale
-- TTY  I / TTY  O (HsColour)
--                 : stdin/stdout are both in locale

-- fully mimic Prelude analogues
writeUTF8File f txt = withFile f WriteMode (\hdl -> do set_utf8_io_enc hdl
                                                       hPutStr hdl txt)
readUTF8File name   = openFile name ReadMode >>=
                      set_utf8_io_enc >>=
                      hGetContents

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile name mode = bracket (openFile name mode) hClose

