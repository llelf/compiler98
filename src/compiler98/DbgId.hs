module DbgId(tTrace, t_R, tSR, {- tSR2, tSR3,-} tDNum, tE,
            t_mkTRoot,t_mkTNm,t_mkTHidden
            ,t_lazySat,t_lazySatLonely,t_eagerSat
            ,t_ap,t_rap,t_tap,t_trap,t_patvar,t_caf
            ,t_fun,t_tfun,t_primn,t_tprimn
            ,t_cn, t_pa, t_con, t_tpa, t_tcon, t_trust, t_rPatBool, t_pap,
	     t_conInt, t_conChar, t_conInteger, t_conRational, t_conDouble, 
	     t_conCons, t_indir,
	     t_conFloat, t_fromConInteger, t_patFromConInteger, t_fatal, 
	     t_fromConRational, t_patFromConRational
	     ,t_debugger, t_prim, t_rseq,t_myseq,t_cSeq
             ,t_mkSR',t_mkNoSR
             ,t_mkNTId',t_mkNTConstr',t_mkSR,t_mkNTId,t_mkNTConstr
             ,t_mkNTLambda,t_mkNTCase,t_mkNTGuard,t_mkNTIf
             ,t_mkTAp
	     ,tNmType
	     ,tDbgPrelude,tDbgPreludeCore,tNmCoerce,tokenDbg)
 where

import IdKind
import TokenId
import PackedString(PackedString, unpackPS, packString)

tTrace		= qualImp "Trace"
t_R		= qualImp "R"
tE		= qualImp "E"
tSR		= qualImp "SR"
tDNum           = qualImp "Num"
t_mkTRoot       = qualImp "mkTRoot"
t_mkTNm         = qualImp "mkTNm"
t_mkNoSR        = qualImp "mkNoSR"
t_mkSR'         = qualImp "mkSR'"
t_mkNTId'       = qualImp "mkNTId'"
t_mkNTConstr'   = qualImp "mkNTConstr'"
t_mkSR          = qualImp "mkSR"
t_mkNTId        = qualImp "mkNTId"
t_mkNTConstr    = qualImp "mkNTConstr"
t_mkNTLambda    = qualImp "mkNTLambda"
t_mkNTCase      = qualImp "mkNTCase"
t_mkTHidden     = qualImp "mkTHidden"
t_mkNTGuard     = qualImp "mkNTGuard"
t_mkNTIf        = qualImp "mkNTIf"
t_mkTAp n       = qualImp ("mkTAp" ++ show n)     
t_lazySat       = qualImp "lazySat"
t_lazySatLonely = qualImp "lazySatLonely"
t_eagerSat      = qualImp "eagerSat"
t_patvar	= qualImp "patvar"
t_caf		= qualImp "caf"
t_fun n		= qualImp ("fun" ++ show n)
t_tfun n        = qualImp ("tfun" ++ show n)
t_ap n		= qualImp ("ap" ++ show n)
t_rap n		= qualImp ("rap" ++ show n)
t_tap n		= qualImp ("tap" ++ show n)
t_trap n	= qualImp ("trap" ++ show n)
t_pap n		= qualImp ("pap" ++ show n)
-- t_c n		= qualImp ("c" ++ show n)
t_cn n		= qualImp ("cn" ++ show n)
t_pa n		= qualImp ("pa" ++ show n)
t_con n		= qualImp ("con" ++ show n)
t_tpa n		= qualImp ("tpa" ++ show n)
t_tcon n	= qualImp ("tcon" ++ show n)
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
t_debugger	= qualImp "debugger"
t_prim	        = qualImp "_prim"
t_rseq	        = qualImp "rseq"
t_cSeq	        = qualImp "cSeq"
t_myseq         = qualImp "myseq"
t_trust	        = qualImp "trust"
tNmType		= qualImp "NmType"
tDbgPrelude	= visImpRev "DPrelude"
tDbgPreludeCore	= visImpRev "DbgPreludeCore"
tNmCoerce	= qualImp "NmCoerce"

qualImp = Qualified rpsDbgPrelude . packString . reverse

rpsDbgPrelude = (packString . reverse ) "Prelude"

tokenDbg = [(TCon, tTrace), 
            (TCon, t_R), 
	    (Con, t_R), 
	    (Con, tE), 
	    (TCon, tSR),
	    (TClass, tDNum),
	    (TClass, tNmCoerce),
            (Var, t_mkTRoot),
            (Var, t_mkTNm),
            (Var, t_mkNoSR),
            (Var, t_mkSR'),
            (Var, t_mkNTId'),
            (Var, t_mkNTConstr'),
            (Var, t_mkSR),
            (Var, t_mkNTId),
            (Var, t_mkNTConstr),
            (Var, t_mkNTLambda),
            (Var, t_mkNTCase),
            (Var, t_mkTHidden),
            (Var, t_mkNTGuard),
            (Var, t_mkNTIf),
            (Var, t_mkTAp 2),
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
            (Var, t_lazySatLonely),
            (Var, t_eagerSat),
	    (Var, t_patvar),
	    (Var, t_fatal), 
	    (Var, t_prim),
	    (Var, t_rseq),
	    (Var, t_cSeq),
            (Var, t_myseq),
	    (Var, t_trust),
	    (TCon, tNmType),
            (Var, tRatioCon),		-- defined in TokenId, not here.
	    (TClass, tIntegral)]	-- likewise.
           ++ [(Var,  t_fun n)   | n <- [0..12]]
           ++ [(Var,  t_tfun n)  | n <- [0..12]]
	   ++ [(Var,  t_ap n)    | n <- [1..12]]
	   ++ [(Var,  t_rap n)   | n <- [1..12]]
	   ++ [(Var,  t_tap n)   | n <- [1..12]]
	   ++ [(Var,  t_trap n)  | n <- [1..12]]
	   ++ [(Var,  t_pap n)   | n <- [1..10]]
	   ++ [(Var,  t_cn n)    | n <- [1..8]]
	   ++ [(Var,  t_pa n)    | n <- [0..4]]
	   ++ [(Var,  t_con n)   | n <- [0..12]]
	   ++ [(Var,  t_tpa n)   | n <- [0..4]]
	   ++ [(Var,  t_tcon n)  | n <- [0..12]]
	   ++ [(Var,  t_primn n) | n <- [0..12]]
	   ++ [(Var,  t_tprimn n)| n <- [0..12]]
	   ++ [(TCon, t_Tuple n) | n <- [2..12]]	   
	   ++ [(Con,  t_Tuple n) | n <- [2..12]]
