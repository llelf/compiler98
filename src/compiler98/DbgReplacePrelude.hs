module DbgReplacePrelude(dbgReplacePrelude) where

import IdKind
import TokenId
import Info
import ImportState
import Flags(sIncludes)
import Extra
import Error(can'tOpenAnyOf)
import Tree234
import AssocTree
import Monad

dbgReplacePrelude flags {-False-} _  impstate = return initTree
{-
dbgReplacePrelude flags True (ImportState _ _ _ _ _ _ rns _ _ _) = 
    let mapfile = "PreludeDbg.map"
        possiblefiles = map (++"/"++mapfile) (sIncludes flags)
    in readMapFile possiblefiles (can'tOpenAnyOf mapfile (sIncludes flags)) >>= \pdm ->
       foldM (replaceEntry rns) initTree (map words (lines pdm))
-}

replaceEntry rns tree [s1, s2] = 
     case lookupAT rns (visImpRev s1, Var) of
	 Nothing -> error ("Couldn't find " ++ s1 ++ " in PreludeDbg.map.")
	 Just n1 ->
	     case lookupAT rns (visImpRev s2, Var) of
		 Nothing -> error ("Couldn't find " ++ s2 ++ " in PreludeDbg.map.")
		 Just n2 ->
		     return (treeAdd (error "No combine function") 
		            compare (uniqueI n1, uniqueI n2) tree)

readMapFile []     dofail = error "Cannot read debug map file, probably no -I or -P"
readMapFile [x]    dofail = catch (readFile x) dofail
readMapFile (x:xs) dofail = catch (readFile x) (\ioe -> readMapFile xs dofail) 

{-
nameFromIdKindHS (fail s1) getNext Var (mkHS s1) bind
    where getNext n = nameFromIdKindHS (fail s2) (wrapEm n) Var (mkHS s2) bind
          wrapEm n1 n2 = treeAdd (error "noway") cmp (getUnique n1, getUnique n2) tree
	  getUnique (Name _ _ u _ _ _ _ _ _ _) = u
          fail s = error ("Couldn't find " ++ show s)
          mkHS s = fst (toHS s hash)
	  cmp (i1, _) (i2, _) c1 c2 c3 = if i1 < i2 then c1 else if i1 == i2 then c2 else c3
-}


