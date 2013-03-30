{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, GADTs, TupleSections  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Annot
-- Copyright   :  (c) Ranjit Jhala
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Write out HsColour compatible type annotations
-----------------------------------------------------------------------------

module Haddock.Backends.Annot ( 
    ppAnnot
  ) where

import GHC              
import GHC.Exts         (groupWith)
import Outputable
import Var              (isId)
import Id               (idName, isDictId)
import NameSet          (NameSet)
import FastString       
import PprTyThing 
import System.IO
import System.FilePath

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Data
import Data.Generics.Aliases hiding (GT)
import Haddock.Types

ppAnnot :: [Interface] -> FilePath -> FilePath -> IO ()
ppAnnot ifaces odir file 
  = do putStrLn $ "Writing Annots to: " ++ target 
       withFile target WriteMode $ \h -> mapM_ (render h) ifaces
    where target = odir </> file

render :: Handle -> Interface -> IO ()
render annH iface 
  = do putStrLn $ "Render Annot: src = " ++ src ++ " module = " ++ modl
       hPutStr annH annots 
    where src     = ifaceOrigFilename iface
          modl    = moduleNameString $ moduleName $ ifaceMod iface
          annots  = show_AnnMap modl $ getAnnMap (fsLit src) $ ifaceTcSource iface 

newtype AnnMap = Ann (M.Map Loc (String, String))                    
type Loc = SrcSpan 

show_AnnMap :: String -> AnnMap -> String
show_AnnMap modl (Ann m)  = "\n\n" ++ (concatMap ppAnn $ M.toList m)
    where ppAnn (l, (x,s)) =  x ++ "\n" 
                           ++ modl ++ "\n"
                           ++ show (srcSpanStartLine l) ++ "\n"
                           ++ show (srcSpanStartCol l)  ++ "\n"
                           ++ show (length $ lines s)   ++ "\n"
                           ++ s ++ "\n\n\n"
          --spanFile         = takeFileName . unpackFS . srcSpanFile 
 
---------------------------------------------------------------------------
-- Extract Annotations ----------------------------------------------------
---------------------------------------------------------------------------

--traceShow :: (Show a) => String -> a -> a
--traceShow s x = trace ("Trace: " ++ s ++ " : " ++ show x) $ x

getAnnMap ::  Data a => FastString -> a -> AnnMap
getAnnMap src tcm  = Ann $ M.fromList $ canonize $ anns
  where anns   = [(l, (s, renderId x)) | (l, (s, x)) <- rs ++ ws ]
        rs     = [(l, (s, x)) | (_,l,s) <- getLocEs tcm, x <- typs s]
        ws     = [(l, (s, x)) | (s, (x, Just l)) <- ns]
        ns     = getNames src tcm
        tm     = M.fromList ns
        typs s = case s `M.lookup` tm of 
                   Nothing    -> []
                   Just (x,_) -> [x]

canonize :: (Ord b, Eq a) => [(b, (t, [a]))] -> [(b, (t, [a]))]
canonize anns = map (head . L.sortBy cmp) $ groupWith fst anns
  where cmp (_,(_,x1)) (_,(_,x2))
          | x1 == x2              = EQ
          | length x1 < length x2 = GT
          | otherwise             = LT

getLocEs ::  (Data a) => a -> [(HsExpr Id, Loc, String)]
getLocEs z = [(e, l, stripParens $ showPpr e) | L l e <- findLEs z]
  where stripParens ('(':s)  = stripParens s
        stripParens s        = stripRParens (reverse s)
        stripRParens (')':s) = stripRParens s
        stripRParens s       = reverse s

getNames ::  (Data a) => FastString -> a -> [(String, (Id, Maybe Loc))]
getNames src z = [(showPpr x, (x, idLoc src x)) | x <- findIds z, idOk x ]
  where idOk = not . isDictId 

renderId :: Id -> String
renderId = showSDocForUser neverQualify . pprTyThing False . AnId

idLoc :: FastString -> Id -> Maybe Loc
idLoc src x 
  | not (isGoodSrcSpan sp) 
  = Nothing 
  | src /= srcSpanFile sp  
  = Nothing
  | otherwise              = Just sp
  where sp  = nameSrcSpan $ idName x

---------------------------------------------------------------------------
-- Visiting and Extracting Identifiers ------------------------------------
-- From Tamar Christina: http://mistuke.wordpress.com/category/vsx --------
---------------------------------------------------------------------------

data Guard where 
  Guard :: Typeable a => Maybe a -> Guard 
  
-- type HList = [Guard]

-- | Summarise all nodes in top-down, left-to-right order 
everythingButQ :: (r -> r -> r) -> [Guard] -> GenericQ r -> GenericQ r 
everythingButQ k q f x 
  = foldl k (f x) fsp 
    where fsp = case isPost x q of 
                  True  -> [] 
                  False -> gmapQ (everythingButQ k q f) x

isPost :: Typeable a => a -> [Guard] -> Bool 
isPost a = or . map check 
  where check :: Guard -> Bool 
        check x = case x of 
                    Guard y -> isJust $ (cast a) `asTypeOf` y

-- | Get a list of all entities that meet a predicate 
listifyBut :: Typeable r => (r -> Bool) -> [Guard] -> GenericQ [r] 
listifyBut p q 
  = everythingButQ (++) q ([] `mkQ` (\x -> if p x then [x] else []))

skipGuards :: [Guard]
skipGuards = [ Guard (undefined :: Maybe NameSet) 
             , Guard (undefined :: Maybe PostTcKind)]

findIds :: Data a => a -> [Id] 
findIds a = listifyBut isId skipGuards a

findLEs :: Data a => a -> [LHsExpr Id] 
findLEs a = listifyBut (isGoodSrcSpan . getLoc) skipGuards a 
