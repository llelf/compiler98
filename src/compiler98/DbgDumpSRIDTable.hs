{- ---------------------------------------------------------------------------
Write SRIDTable into assembler file.
-}
module DbgDumpSRIDTable(dbgDumpSRIDTable) where

import Char
import IO
import System
import PackedString(PackedString, unpackPS, packString)
import IntState
import Extra(strStr, dropJust, trace)
import Flags
import Syntax(ImpDecl(..), ImpSpec(..), Entity(..), InfixClass(..))
import TokenId(TokenId(..))
import DbgTrans(SRIDTable)

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

dbgDumpSRIDTable :: Handle -> IntState -> Flags -> SRIDTable -> IO ()

dbgDumpSRIDTable handle state flags Nothing = return ()
dbgDumpSRIDTable handle state flags (Just ((_, srs), idt, impdecls, modid)) = 
    output (showString "DL(" . showString modpre . showString ")\n" . 
            chopString filename .
	    showString "  AL\n  EX L(NM_" . showString srcid . 
            showString ")\n DL(NM_" . showString srcid . showString ")\n") >>
    mapM_ (dumpId state (sProfile flags) modinfo output trust) idtlabs >>
    output (showString "  DW 0\n") >>
    mapM_ (dumpNs state (sProfile flags) modpre output trust) idtlabs >>
    output (showString "  AL\nDL(D_srstart)\n") >>
    mapM_ (dumpSR state (sProfile flags) modinfo output) (reverse srs) >>
    output (showString " DL(N_IMPORTS)\n") >>
    mapM_ (dumpImport output) impdecls >>
    output (showString "  DW 0\n  EX L(" . showString modinfo .
            showString ")\n DL(" . showString modinfo .
	    showString ")\n  DW L(" . showString modpre .
            showString "), L(NM_" . showString srcid .
            showString "), L(N_IMPORTS), L(NMODN)\n" .
            showString " DW 0\n" .
	    showString " DL(NMODN)\n" . chopString modid . 
	    showString "  AL\n") >>
    if modid == "Main" then
        output (showString "  EX L(MODULE_Main)\n DL(MODULE_Main)\n DW L(" .
	        showString modinfo . showString ")\n")
    else
        return ()
    where modpre = "D_" ++ srcid
          modinfo = "NMOD_" ++ srcid
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
          filename = let ms = sSourceFile flags
		     in reverse (takeWhile ('/' /=) (reverse ms))
          srcid = let ms = sSourceFile flags
	              ms' = case break ('.'==) (reverse ms) of
			      ("sh", rf) -> reverse (tail rf)
			      ("shl", rf) -> reverse (tail rf)
			      _ -> ms
		  in reverse (takeWhile ('/' /=) (reverse ms'))
dumpId state profile modinfo output trust ((pos, i, tid), lab) =
    output (showString "  EX L(D_" . showString idnhc . 
            showString ")\nDL(D_" . showString idnhc . 
	    -- The 6 below is the constructor number of NTId
	    -- 22 (16+6) is used if the function is trusted
	    -- See getconstr.h in the runtime system.
	    (if trust && isVar then
	        showString ")\n  DW CONSTR(22,3,3)\n  DW " 
	     else
	        showString ")\n  DW CONSTR(6,3,3)\n  DW ") . 
	    (if profile then showString "L(prof_NTId), 0, 0, 0, " else id) .
	    showString "L(" . showString modinfo .
            showString "), " . shows pos . 
	    showString ", L(L_" . shows lab . showString "), " .
	    shows (priority pri) . showString"\n" .
            showString "  DW 0\n")
    where idnhc = fixStr (show tid) ""
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

dumpNs state profile modpre output trust ((pos, _, tid), lab) =
    output (showString "DL(L_" . shows lab . showString ")\n" .
            chopString (untoken tid))

dumpSR state profile modinfo output sr =
    -- (2, 2, 2) -> (Tag 2 (SR3), size 2, 2 non-pointers)
    output (showString "  DW CONSTR(2,2,2)\n  DW "
           . (if profile then showString "L(prof_SR3), 0, 0, 0, " else id)
           . shows sr . showString ", L(" . showString modinfo
           . showString ")\n  DW 0\n")

dumpImport output impdecl = 
{-
    if modname == "DPrelude" || take 7 (modname ++ "      ") == "Prelude" then 
        return ()
    else
-}
    output (showString "  DW L(NMOD_" .  showString modname . showString ")\n")
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

chopString "" = showString "  DB 0\n"
chopString x  = 
    case splitAt (40::Int) x of
        (x,xs) -> showString "  DS " . showString (strStr x) . 
	          showString "\n" . chopString xs

{- End DbgDumpSRIDTable ------------------------------------------------------}
