{- ---------------------------------------------------------------------------
Write SRIDTable into C array declarations.
-}
module DbgDumpSRIDTableC(dbgDumpSRIDTableC) where

import Char
import IO
import System
import PackedString(PackedString, unpackPS, packString)
import IntState
import Extra(strStr,dropJust,trace)
import Flags
import GcodeLow(fixStr)
import Syntax(ImpDecl(..), ImpSpec(..), Entity(..), InfixClass(..))
import TokenId(TokenId(..))
import EmitState
import DbgTrans(SRIDTable,LevelId(..))

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

dbgDumpSRIDTableC :: Pass -> Handle -> IntState -> Flags -> SRIDTable 
                  -> EmitState -> EmitState

dbgDumpSRIDTableC p handle state flags Nothing = id
dbgDumpSRIDTableC p handle state flags (Just ((_, srs), idt, impdecls, modid)) =
    -- Sourcefile name
    emitAlign p >|>
    defineLabel p Local (modpre) >|>
    emitString p filename >|>
    -- Module name
    emitAlign p >|>
    defineLabel p Local ("NMODN") >|>
    emitString p modid >|>
    -- Identifier table (strings)
    emitAlign p >|>
    foldr (>|>) (emitWord p ("0")) (map (emitName p) idtlabs) >|>
    -- Name table
    emitAlign p >|>
    defineLabel p Global ("NM_" ++ srcid) >|>
    foldr (>|>) id (map (emitId p) idtlabs) >|>
    emitWord p ("0") >|>
    -- Import table
    defineLabel p Local ("N_IMPORTS") >|>
--  foldr (>|>) (emitWord p ("0")) (map (emitImport p) impdecls) >|>
                 emitWord p ("0")                                >|>
    -- Module record
    defineLabel p Global (modinfo) >|>
    useLabel p (modpre) >|>
    useLabel p ("NM_" ++ srcid) >|>
    useLabel p ("N_IMPORTS") >|>
    useLabel p ("NMODN") >|>
    emitWord p ("0") >|>
    emitWord p (if trust then "1" else "0") >|>
    -- Source references
    emitAlign p >|>
    foldr (>|>) id (map (emitSR p) (zip [0..] (reverse srs))) >|>
    -- special Main record
    if modid == "Main" then
        defineLabel p Global ("MODULE_Main") >|>
        useLabel p (modinfo)
    else
        id
    where profile = sProfile flags
          modpre = "D_" ++ srcid
          modinfo = "NMOD_" ++ srcid
          trust = sDbgTrusted flags
	  idtlabs = zip ( [(True,  p, i, (tidI . dropJust . lookupIS state) i) 
	                  | TopId (p, i) <- idt ] ++
	                  [(False, p, i, (tidI . dropJust . lookupIS state) i) 
	                  | LocalId (p, i) <- idt ] )
			[0..]
          output sf = catch (hPutStr handle (sf "")) outputerr
	  outputerr ioerror = hPutStr stderr ("Failed appending debug tables to"
	                                      ++sObjectFile flags++":" ++ 
					      show ioerror ++ "\n") 
			      >> exitWith (ExitFailure (-1))
          filename = let ms = sSourceFile flags
		     in reverse (takeWhile ('/' /=) (reverse ms))
          srcid = let ms = sSourceFile flags
	              ms' = case break ('.'==) (reverse ms) of
			      ("sh", rf) -> reverse (tail rf)
			      ("shl", rf) -> reverse (tail rf)
			      ("gc", rf) -> reverse (tail rf)
			      _ -> ms
		  in fixStr (reverse (takeWhile ('/' /=) (reverse ms'))) ""

          emitName p ((top, pos, _, tid), lab) =
	      defineLabel p Local ("L_" ++ show lab) >|>
	      emitString p (untoken tid)
	  emitSR p (ix, sr) =
	      -- (2, 2, 2) -> (Tag 2 (SR3), size 2, 2 non-pointers)
	      defineLabel p Local ("D_SR_" ++ show ix) >|>
	      emitWord p ("CONSTR(2,2,2)") >|>
	      (if profile then 
	          useLabel p ("prof_SR3") >|>
		  emitWord p (show 0) >|>
		  emitWord p (show 0) >|>
		  emitWord p (show 0)
	       else
	          id) >|>
	      emitWord p (show sr) >|>
              useLabel p (modinfo) >|>
	      emitWord p ("0")
          emitId p ((top, pos, i, tid), lab) =
	      defineLabel p Global ("D_" ++ idnhc) >|>
	  --  -- The 6 below is the constructor number of NTId
	  --  -- 22 (16+6) is used if the function is trusted
	  --  -- See getconstr.h in the runtime system.
	  --  (if trust && isVar then
	  --       emitWord p ("CONSTR(22,5,5)")
	  --   else
	  --       emitWord p ("CONSTR(6,5,5)")) >|>
              -- Representation is changed to 22==toplevel 6==local identifier
	      (if isVar && top then
	           emitWord p ("CONSTR(22,5,5)")
	       else
	           emitWord p ("CONSTR(6,5,5)")) >|>
	      (if profile then 
	          useLabel p ("prof_NTId") >|>
		  emitWord p (show 0) >|>
		  emitWord p (show 0) >|>
		  emitWord p (show 0)
	       else
	          id) >|>
              useLabel p (modinfo) >|>
	      emitWord p (show pos) >|>
	      useLabel p ("L_" ++ show lab) >|>
	      emitWord p (show (priority pri)) >|>
              emitWord p ("0")
	    where
	      idnhc = fixStr (show tid) ""
	      (isVar, pri) = 
	          case lookupIS state i of
	            Just (i@(InfoConstr _ _ _ _ _ _ _)) -> (False, fixityI i)
		    Just (i@(InfoIMethod _ _ _ _ m)) -> 
	      	      case lookupIS state m of 
	      	        Just im -> (True, fixityI im)
		    Just i -> (True, fixityI i)
		    _ -> (True, (InfixL, 9))
	      priority :: (InfixClass TokenId, Int) -> Int
	      priority (InfixDef, _)   = 3
	      priority (InfixL, n)     = 2 + shiftPri n
	      priority (InfixR, n)     = 1 + shiftPri n
	      priority (Infix, n)      = 0 + shiftPri n
	      priority (InfixPre _, n) = 0 + shiftPri n
	      shiftPri :: Int -> Int
	      shiftPri n = n * 4
	  emitImport p impdecl = 
	      useLabel p ("NMOD_" ++ modname)
	      where modname = untoken (imptokid impdecl)
	            imptokid (Import (_,i) _) = i
		    imptokid (ImportQ (_,i) _) = i
		    imptokid (ImportQas (_,i) _ _) = i
		    imptokid (Importas (_,i) _ _) = i

untoken (TupleId 0) = ""
untoken (TupleId n) = take (n-1) (repeat ',')
untoken (Visible ps) = reverse (unpackPS ps)
untoken (Qualified _ ps) = reverse (unpackPS ps)
untoken (Qualified2 _ tid) = untoken tid
untoken (Qualified3 _ _ tid) = untoken tid

{- --------------------------------------------------------------------------}
