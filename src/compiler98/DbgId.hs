module DbgId(tTrace, t_R, tSR, {- tSR2, tSR3,-} tDNum, tE,
             t_mkTRoot,t_mkTNm,t_hide,
             {- t_Ap, t_Nm, t_Ind, t_Root, t_Sat, t_Pruned, t_Hidden,-}
             t_lazySat,
             t_ap, t_rap, t_patvar, t_caf, t_fun, t_tfun, t_primn, t_tprimn
            , t_guard, t_if, t_rif
            ,t_cn, t_pa, t_con, t_trust, t_rPatBool, t_pap,
	     t_conInt, t_conChar, t_conInteger, t_conRational, t_conDouble, 
	     t_conCons, t_indir,
	     t_conFloat, t_fromConInteger, t_patFromConInteger, t_fatal, 
	     t_fromConRational, t_patFromConRational
	     ,t_debugger, t_prim, t_rseq, t_cSeq
             ,t_mkSR',t_mkNTId',t_mkNTConstr',t_mkSR,t_mkNTId,t_mkNTConstr
             ,t_mkNTLambda,t_mkNTCase
	     ,tNTId, tNTConstr, tCase, tIf, tGuard, tLambda, tNmType,
	     tDbgPrelude,tDbgPreludeCore,tNmCoerce,tokenDbg)
 where

import IdKind
import TokenId
import PackedString(PackedString, unpackPS, packString)

tTrace		= qualImp "Trace"
t_R		= qualImp "R"
tE		= qualImp "E"
tSR		= qualImp "SR"
--tSR2		= qualImp "SR2"
--tSR3		= qualImp "SR3"
tDNum           = qualImp "Num"
t_mkTRoot       = qualImp "mkTRoot"
t_mkTNm         = qualImp "mkTNm"
t_mkSR'         = qualImp "mkSR'"
t_mkNTId'       = qualImp "mkNTId'"
t_mkNTConstr'   = qualImp "mkNTConstr'"
t_mkSR          = qualImp "mkSR"
t_mkNTId        = qualImp "mkNTId"
t_mkNTConstr    = qualImp "mkNTConstr"
t_mkNTLambda    = qualImp "mkNTLambda"
t_mkNTCase      = qualImp "mkNTCase"
t_hide          = qualImp "hide"
--t_Ap		= qualImp "Ap"
--t_Nm		= qualImp "Nm"
--t_Ind		= qualImp "Ind"
--t_Root		= qualImp "Root"
--t_Sat		= qualImp "Sat"
--t_Pruned       	= qualImp "Pruned"
--t_Hidden       	= qualImp "Hidden"
t_lazySat       = qualImp "lazySat"
t_patvar	= qualImp "patvar"
t_caf		= qualImp "caf"
t_guard		= qualImp "t_guard"
t_if		= qualImp "tif"
t_rif           = qualImp "trif"
t_fun n		= qualImp ("fun" ++ show n)
t_tfun n        = qualImp ("tfun" ++ show n)
t_ap n		= qualImp ("ap" ++ show n)
t_rap n		= qualImp ("rap" ++ show n)
t_pap n		= qualImp ("pap" ++ show n)
-- t_c n		= qualImp ("c" ++ show n)
t_cn n		= qualImp ("cn" ++ show n)
t_pa n		= qualImp ("pa" ++ show n)
t_con n		= qualImp ("con" ++ show n)
t_primn n	= qualImp ("prim" ++ show n)
t_tprimn n      = qualImp ("tprim" ++ show n)
t_indir		= qualImp "indir"
t_conInt	= qualImp "conInt"
t_conChar	= qualImp "conChar"
t_conInteger	= qualImp "conInteger"
t_conRational	= qualImp "conRational"
t_conDouble	= qualImp "conDouble"
t_conFloat	= qualImp "conFloat"
t_conCons       = qualImp "conCons"
t_fromConInteger= qualImp "fromConInteger"
t_patFromConInteger = qualImp "patFromConInteger"
t_fromConRational = qualImp "fromConRational"
t_patFromConRational = qualImp "patFromConRational"
t_rPatBool      = qualImp "rPatBool"
t_fatal		= qualImp "fatal"
-- t_dbgerror     	= qualImp "_error"
t_debugger	= qualImp "debugger"
t_prim	        = qualImp "_prim"
t_rseq	        = qualImp "rseq"
t_cSeq	        = qualImp "cSeq"
t_trust	        = qualImp "trust"
-- tRString	= qualImp "RString"
-- tList		= qualImp "RList"
-- tCons		= qualImp "RCons"
-- tNil		= qualImp "RNil"
tNmType		= qualImp "NmType"
tNTConstr	= qualImp "NTConstr"
tNTId		= qualImp "NTId"
tCase		= qualImp "NTCase"
tIf		= qualImp "NTIf"
tGuard		= qualImp "NTGuard"
tLambda		= qualImp "NTLambda"
tDbgPrelude	= visImpRev "DPrelude"
tDbgPreludeCore	= visImpRev "DbgPreludeCore"
-- t_dbginteract	= qualImp "interact"
-- t_dbgprint	= qualImp "dbgprint"
-- t_stringConst	= qualImp "stril, t_patFromConRational,
tNmCoerce	= qualImp "NmCoerce"

qualImp = Qualified rpsDbgPrelude . packString . reverse

rpsDbgPrelude = (packString . reverse ) "Prelude"

tokenDbg = [(TCon, tTrace), 
            (TCon, t_R), 
	    (Con, t_R), 
	    (Con, tE), 
	    (TCon, tSR),
	 -- (Con, tSR),
	 -- (Con, tSR2),
	 -- (Con, tSR3),
	    (TClass, tDNum),
	    (TClass, tNmCoerce),
            (Var, t_mkTRoot),
            (Var, t_mkTNm),
            (Var, t_mkSR'),
            (Var, t_mkNTId'),
            (Var, t_mkNTConstr'),
            (Var, t_mkSR),
            (Var, t_mkNTId),
            (Var, t_mkNTConstr),
            (Var, t_mkNTLambda),
            (Var, t_mkNTCase),
            (Var, t_hide),
	 -- (Con, t_Ap),
	 -- (Con, t_Nm),
	 -- (Con, t_Ind), 
	 -- (Con, t_Root), 
	 -- (Con, t_Sat), 
	 -- (Con, t_Pruned), 
	 -- (Con, t_Hidden), 
	    (Var, t_indir),
	    (Var, t_conInt),
	    (Var, t_conChar),
	    (Var, t_conInteger),
	    (Var, t_conRational),
	    (Var, t_conFloat),
	    (Var, t_conDouble),
            (Var, t_conCons),
	    (Var, t_fromConInteger),
	    (Var, t_patFromConInteger),
	    (Var, t_fromConRational),
	    (Var, t_patFromConRational),
	    (Var, t_rPatBool),
	    (Var, t_caf), 
            (Var, t_lazySat),
	    (Var, t_patvar),
	    (Var, t_fatal), 
--	    (Var, t_dbgerror),
	    (Var, t_prim),
	    (Var, t_rseq),
	    (Var, t_cSeq),
	    (Var, t_trust),
	    (TCon, tNmType),
	 -- (Con, tNTConstr),
	 -- (Con, tNTId),
	 -- (Con, tCase),
	 -- (Con, tIf),
	 -- (Con, tGuard),
	 -- (Con, tLambda),
--	    (TSyn, tRString), 
--	    (TCon, tList), 
--	    (Con, tCons), 
--	    (Con, tNil),
            (Var, tRatioCon),		-- defined in TokenId, not here.
	    (TClass, tIntegral),	-- likewise.
--	    ,(Modid, tDbgPrelude)
--	    ,(Modid, tDbgPreludeCore)
--	    (Var, t_dbginteract), 
--	    (Var, t_stringConst), 
--	    (Var, t_dbgprint),
	    (Var, t_if),
            (Var, t_rif),
	    (Var, t_guard)] 
           ++ [(Var,  t_fun n)   | n <- [0..12]]
           ++ [(Var,  t_tfun n)  | n <- [0..12]]
	   ++ [(Var,  t_ap n)    | n <- [1..12]]
	   ++ [(Var,  t_rap n)   | n <- [1..12]]
	   ++ [(Var,  t_pap n)   | n <- [1..10]]
--	   ++ [(Var,  t_c n)     | n <- [1..12]]
	   ++ [(Var,  t_cn n)    | n <- [1..8]]
	   ++ [(Var,  t_pa n)    | n <- [0..4]]
	   ++ [(Var,  t_con n)   | n <- [0..12]]
	   ++ [(Var,  t_primn n)  | n <- [0..12]]
	   ++ [(Var,  t_tprimn n) | n <- [0..12]]
	   ++ [(TCon, t_Tuple n) | n <- [2..12]]	   
	   ++ [(Con,  t_Tuple n) | n <- [2..12]]
