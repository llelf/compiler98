module Nice (fixTid, mkAL, mkALNT
	, niceCtxs, niceField, niceInt, niceNT
	, niceNewType, niceTid, showsOp, showsVar
	)  where

import NT
import IntState
import Extra(dropJust,mixComma,mixSpace,assocDef,snub,strace)
import PackedString()
import TokenId

niceNT _ state al (NTany  a) = assocDef al ('?':show a ++ "?") a
niceNT _ state al (NTvar  a) = assocDef al ('?':show a ++ "?") a
niceNT _ state al (NTexist a) = assocDef al ('?':show a ++ "?") a
niceNT m state al (NTstrict t) = "!" ++ niceNT m state al t
niceNT m state al (NTapp t1 t2) = 
   '(':  niceNT m state al t1 ++ ' ': niceNT m state al t2 ++ ")"
niceNT m state al (NTcons a []) = niceInt m state a ""
niceNT m state al (NTcons a tas) =
        case (tidI . dropJust .  lookupIS state) a of
	  TupleId _ -> '(' : mixComma (map (niceNT m state al) tas) ++ ")"
	  v | v == t_Arrow ->
		case tas of
		 [] -> "(->)"
		 [t1] -> "( (->) " ++ niceNT m state al t1 ++ ")"
		 [t1,t2] -> '(':niceNT m state al t1 ++ " -> " ++ niceNT m state al t2++")"
	  v | v == t_List  -> "[" ++ (case tas of [] -> ""; [t] -> niceNT m state al t) ++ "]"
	  v -> '(': show (fixTid (mrpsIS state) v) ++ ' ': mixSpace (map (niceNT m state al) tas) ++ ")"
niceNT m state al (NTcontext c a) =
        case (tidI . dropJust .  lookupIS state) c of
	  TupleId _ -> '(' : niceNT m state al (NTvar a) ++ ")"
	  v -> '(': show (fixTid (mrpsIS state) v) ++ ' ': niceNT m state al (NTvar a) ++ ")"


niceCtxs mmrps state al [] = ""
niceCtxs mmrps state al ctxs = "(" ++ mixComma (map ( \ (c,v) -> niceInt mmrps state c  (' ':assocDef al (error "niceCtx") v)) ctxs) ++ ") => "

niceInt Nothing state i     = (niceInfo (mrpsIS state) . dropJust . lookupIS state) i
niceInt (Just mrps) state i = (niceInfo  mrps          . dropJust . lookupIS state) i

niceTid state tid = (showsVar . fixTid (mrpsIS state)) tid

niceInfo mrps info = (showsVar . fixTid mrps . tidI) info



fixTid mrps (Qualified tid n) | tid == mrps = Visible n
fixTid mrps v = v

mkALNT = mkAL . freeNT

mkAL tvs = 
   let tvars = map (:[]) ['a'..'z'] ++ map (++"'") tvars
   in zip tvs tvars

niceNewType state (NewType free exist ctx nts) =
    let
      al = mkAL free ++ zip (map snd ctx) (map (('_':).(:[])) ['a'..'z']) -- a-z is to short!
    in niceCtxs Nothing state al ctx ++ mixSpace (map (niceNT Nothing state al) nts)


showsOp tid =
	if isTidOp tid 
	then shows tid
	else showChar '`' . shows tid . showChar '`'
  
showsVar tid =
	if isTidOp tid 
	then showChar '(' . shows tid . showChar ')'
	else shows tid
  
niceField state al (Just i,nt) = (showChar '{' . shows (fixTid (mrpsIS state) (tidIS state i)) . showString " :: ")
				 (niceNT Nothing state al nt ++ "}")
niceField state al (Nothing,nt) =
   niceNT Nothing state al nt
