module DbgId(tTrace, tR, tSR, tSR2, tSR3, tDNum, t_value, tE,
             t_Ap, t_Nm, t_Ind, t_Root, t_Sat, t_Pruned, t_Hidden,
             t_ap, t_rap, t_patvar, t_caf, t_fun, t_guard, t_if,
	     t_c, t_cn, t_pa, t_con, t_trust,
	     t_conInt, t_conChar, t_conInteger, t_conRational, t_conDouble, 
	     t_indir,
	     t_conFloat, t_fromConInteger, t_patFromConInteger, t_fatal, 
	     t_dbgerror, t_debugger, t_prim, t_rseq, t_cSeq, tRString, tList, 
	     tCons, tNil, tNTId, tNTConstr, tCase, tIf, tGuard, tLambda, tNmType,
	     tDbgPrelude, tDbgPreludeCore, 
	     t_dbginteract, t_dbgprint, t_stringConst,
	     tokenDbg)
 where

import Kind
import TokenId
import PackedString(PackedString, unpackPS, packString)

tTrace		= qualImp "Trace"
tR		= qualImp "R"
tE		= qualImp "E"
tSR		= qualImp "SR"
tSR2		= qualImp "SR2"
tSR3		= qualImp "SR3"
tDNum           = qualImp "Num"
t_value		= qualImp "value"
t_Ap		= qualImp "Ap"
t_Nm		= qualImp "Nm"
t_Ind		= qualImp "Ind"
t_Root		= qualImp "Root"
t_Sat		= qualImp "Sat"
t_Pruned       	= qualImp "Pruned"
t_Hidden       	= qualImp "Hidden"
t_patvar	= qualImp "patvar"
t_caf		= qualImp "caf"
t_guard		= qualImp "guard"
t_if		= qualImp "tif"
t_fun n		= qualImp ("fun" ++ show n)
t_ap n		= qualImp ("ap" ++ show n)
t_rap n		= qualImp ("rap" ++ show n)
t_c n		= qualImp ("c" ++ show n)
t_cn n		= qualImp ("cn" ++ show n)
t_pa n		= qualImp ("pa" ++ show n)
t_con n		= qualImp ("con" ++ show n)
t_indir		= qualImp "indir"
t_conInt	= qualImp "conInt"
t_conChar	= qualImp "conChar"
t_conInteger	= qualImp "conInteger"
t_conRational	= qualImp "conRational"
t_conDouble	= qualImp "conDouble"
t_conFloat	= qualImp "conFloat"
t_fromConInteger= qualImp "fromConInteger"
t_patFromConInteger = qualImp "patFromConInteger"
t_fatal		= qualImp "fatal"
t_dbgerror     	= qualImp "_error"
t_debugger	= qualImp "debugger"
t_prim	        = qualImp "_prim"
t_rseq	        = qualImp "rseq"
t_cSeq	        = qualImp "cSeq"
t_trust	        = qualImp "trust"
tRString	= qualImp "RString"
tList		= qualImp "RList"
tCons		= qualImp "RCons"
tNil		= qualImp "RNil"
tNmType		= qualImp "NmType"
tNTConstr	= qualImp "NTConstr"
tNTId		= qualImp "NTId"
tCase		= qualImp "NTCase"
tIf		= qualImp "NTIf"
tGuard		= qualImp "NTGuard"
tLambda		= qualImp "NTLambda"
tDbgPrelude	= visImpRev "DPrelude"
tDbgPreludeCore	= visImpRev "DbgPreludeCore"
t_dbginteract	= qualImp "interact"
t_dbgprint	= qualImp "dbgprint"
t_stringConst	= qualImp "stringConst"

qualImp = Qualified rpsDbgPrelude . packString . reverse

rpsDbgPrelude = (packString . reverse ) "Prelude"

tokenDbg = [(TCon, tTrace), 
            (TCon, tR), 
	    (Con, tR), 
	    (Con, tE), 
	    (TCon, tSR),
	    (Con, tSR),
	    (Con, tSR2),
	    (Con, tSR3),
	    (TClass, tDNum),
	    (Var, t_value), 
	    (Con, t_Ap),
	    (Con, t_Nm),
	    (Con, t_Ind), 
	    (Con, t_Root), 
	    (Con, t_Sat), 
	    (Con, t_Pruned), 
	    (Con, t_Hidden), 
	    (Var, t_indir),
	    (Var, t_conInt),
	    (Var, t_conChar),
	    (Var, t_conInteger),
	    (Var, t_conRational),
	    (Var, t_conFloat),
	    (Var, t_conDouble),
	    (Var, t_fromConInteger),
	    (Var, t_patFromConInteger),
	    (Var, t_caf), 
	    (Var, t_patvar),
	    (Var, t_fatal), 
	    (Var, t_dbgerror),
	    (Var, t_prim),
	    (Var, t_rseq),
	    (Var, t_cSeq),
	    (Var, t_trust),
	    (TCon, tNmType),
	    (Con, tNTConstr),
	    (Con, tNTId),
	    (Con, tCase),
	    (Con, tIf),
	    (Con, tGuard),
	    (Con, tLambda),
	    (TSyn, tRString), 
	    (TCon, tList), 
	    (Con, tCons), 
	    (Con, tNil),
--	    ,(Modid, tDbgPrelude)
--	    ,(Modid, tDbgPreludeCore)
	    (Var, t_dbginteract), 
	    (Var, t_stringConst), 
	    (Var, t_dbgprint),
	    (Var, t_if),
	    (Var, t_guard)] 
           ++ [(Var,  t_fun n)   | n <- [0..12]]
	   ++ [(Var,  t_ap n)    | n <- [1..12]]
	   ++ [(Var,  t_rap n)   | n <- [1..12]]
	   ++ [(Var,  t_c n)     | n <- [1..12]]
	   ++ [(Var,  t_cn n)    | n <- [1..8]]
	   ++ [(Var,  t_pa n)    | n <- [0..4]]
	   ++ [(Var,  t_con n)   | n <- [0..12]]
	   ++ [(TCon, t_Tuple n) | n <- [2..12]]	   
	   ++ [(Con,  t_Tuple n) | n <- [2..12]]
