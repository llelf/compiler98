module DbgDumpSRIDTableC(dbgDumpSRIDTableC) where

import Char
import IO
import System
import PackedString(PackedString, unpackPS, packString)
import IntState
import Extra(strStr, dropJust)
import Flags
import Syntax(ImpDecl(..), ImpSpec(..), Entity(..), InfixClass(..))
import TokenId(TokenId(..))
import EmitState
#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#endif
#if defined(__GLASGOW_HASKELL__)
import IOExts (trace)
#endif

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

dbgDumpSRIDTableC :: Handle -> IntState -> Flags -> Maybe ((Int, [Int]), [(Pos, Int)], [ImpDecl TokenId], String) -> EmitState -> EmitState
dbgDumpSRIDTableC handle state flags Nothing = id
dbgDumpSRIDTableC handle state flags (Just ((_, srs), idt, impdecls, modid)) = 
    -- Sourcefile name
    emitAlign >|>
    defineLabel Local (showString modpre) >|>
    emitString srcid >|>
    -- Module name
    emitAlign >|>
    defineLabel Local (showString "NMODN") >|>
    emitString modid >|>
    -- Identifier table (strings)
    emitAlign >|>
    foldr (>|>) (emitWord (shows 0)) (map emitName idtlabs) >|>
    -- Source references
    emitAlign >|>
    foldr (>|>) id (map emitSR (zip [0..] (reverse srs))) >|>
    -- Name table
    defineLabel Global (showString "NM_" . showString srcid) >|>
    foldr (>|>) id (map emitId idtlabs) >|>
    emitWord (shows 0) >|>
    -- Import table
    defineLabel Local (showString "N_IMPORTS") >|>
    foldr (>|>) (emitWord (shows 0)) (map emitImport impdecls) >|>
    -- Module record
    defineLabel Global (showString "NMOD_" . showString srcid) >|>
    useLabel (showString modpre) >|>
    useLabel (showString "NM_" . showString srcid) >|>
    useLabel (showString "N_IMPORTS") >|>
    useLabel (showString "NMODN") >|>
    if modid == "Main" then
        defineLabel Global (showString "MODULE_Main") >|>
        useLabel (showString "NMOD_" . showString srcid)
    else
        id
    where profile = sProfile flags
          modpre = "D_" ++ srcid
          trust = sDbgTrusted flags
	  idtlabs = zip [(p, i, (tidI . dropJust . lookupIS state) i) 
	                | (p, i) <- idt
			] 
			[0..]
          output sf = catch (hPutStr handle (sf "")) outputerr
	  outputerr ioerror = hPutStr stderr ("Failed appending debug tables to"
	                                      ++sObjectFile flags++":" ++ 
					      show ioerror ++ "\n") 
			      >> exitWith (ExitFailure (-1))
          srcid = let ms = sSourceFile flags
	              ms' = case break ('.'==) (reverse ms) of
			      ("sh", rf) -> reverse (tail rf)
			      ("shl", rf) -> reverse (tail rf)
			      _ -> ms
		  in reverse (takeWhile ('/' /=) (reverse ms'))

          emitName ((pos, _, tid), lab) =
	      defineLabel Local (showString "L_" . shows lab) >|>
	      emitString (untoken tid)
	  emitSR (ix, sr) =
	      -- (2, 2, 2) -> (Tag 2 (SR3), size 2, 2 non-pointers)
	      defineLabel Local (showString "D_SR_" . shows ix) >|>
	      emitWord (showString "CONSTR(2,2,2)") >|>
	      (if profile then 
	          useLabel (showString "prof_SR3") >|>
		  emitWord (shows 0) >|>
		  emitWord (shows 0) >|>
		  emitWord (shows 0)
	       else
	          id) >|>
	      emitWord (shows sr) >|>
	      useLabel (showString modpre)
          emitId ((pos, i, tid), lab) =
	      defineLabel Global (showString "D_" . showString idnhc) >|>
	      -- The 6 below is the constructor number of NTId
	      -- 22 (16+6) is used if the function is trusted
	      -- See getconstr.h in the runtime system.
	      (if trust && isVar then
	           emitWord (showString "CONSTR(22,3,3)")
	       else
	           emitWord (showString "CONSTR(6,3,3)")) >|>
	      (if profile then 
	          useLabel (showString "prof_NTId") >|>
		  emitWord (shows 0) >|>
		  emitWord (shows 0) >|>
		  emitWord (shows 0)
	       else
	          id) >|>
	      useLabel (showString modpre) >|>
	      emitWord (shows pos) >|>
	      useLabel (showString "L_" . shows lab) >|>
	      emitWord (shows (priority pri))
	    where
	      idnhc = fixStr (show tid) ""
	      (isVar, pri) = 
	          case lookupIS state i of
	            Just (i@(InfoConstr _ _ _ _ _ _)) -> (False, fixityI i)
		    Just (i@(InfoIMethod _ _ _ _ m)) -> 
	      	      case lookupIS state m of 
	      	        Just im -> (True, fixityI im)
		    Just i -> (True, fixityI i)
		    _ -> (True, (InfixL, 9))
	      fixStr [] = id
	      fixStr (c:cs) =
	          if isAlphanum c then
	              showChar c . fixStr cs
	          else 
	              showChar '_' . shows (fromEnum c) . fixStr cs
	      priority :: (InfixClass TokenId, Int) -> Int
	      priority (InfixDef, _)   = 3
	      priority (InfixL, n)     = 2 + shiftPri n
	      priority (InfixR, n)     = 1 + shiftPri n
	      priority (Infix, n)      = 0 + shiftPri n
	      priority (InfixPre _, n) = 0 + shiftPri n
	      shiftPri :: Int -> Int
	      shiftPri n = n * 4
	  emitImport impdecl = 
	      useLabel (showString "NMOD_" . showString modname)
	      where modname = untoken (imptokid impdecl)
	            imptokid (Import (_,i) _) = i
		    imptokid (ImportQ (_,i) _) = i
		    imptokid (ImportQas (_,i) _ _) = i
		    imptokid (Importas (_,i) _ _) = i

untoken (TupleId n) = take n (repeat ',')
untoken (Visible ps) = reverse (unpackPS ps)
untoken (Qualified _ ps) = reverse (unpackPS ps)
untoken (Qualified2 _ tid) = untoken tid
untoken (Qualified3 _ _ tid) = untoken tid
