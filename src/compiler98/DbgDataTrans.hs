module DbgDataTrans(dbgDataTrans) where

import Tree234
import Extra
import IdKind
import TokenId
import DbgId
import IntState
import Syntax
import SyntaxPos
import StrSyntax
import Flags
import Bind
import RenameLib
import State
import NT
import Nice(niceCtxs, niceNT, mkAL)
import AssocTree
import Char 
import PackedString(PackedString, unpackPS, packString)
#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#endif
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import IOExts (trace)
#endif

data Inherited = Inherited ((TokenId, IdKind) -> Int) [(Int, Int)] Int (Tree (Int, Int)) Bool
data Threaded = Threaded IntState [(Int,[(Pos,Int)])] [(Pos, Int)]

dbgDataTrans flags state reptree lookupPrel derived dptopdecls =
  if (sDbgTrans flags) then
      --trace (show derived) $
      case dTopDecls dptopdecls
		(Inherited lookupPrel [] 0 reptree (sDbg2 flags)) (Threaded state derived []) of
          (decls', Threaded state' derived' constrs) -> (decls', derived', state', Just constrs)
  else
      (dptopdecls, derived, state, Nothing)

dTopDecls (DeclsParse ds) = 
    getArities ds >=>
    unitS DeclsParse =>>> (mapS dTopDecl ds >>>= \dss -> unitS (concat dss))

--dTopDecl d@(DeclType (Simple tid _ _) t) = 
dTopDecl d@(DeclAnnot (DeclIgnore _) [AnnotArity (_, tid) _]) =
    lookupName noPos tid >>>= \(Just (InfoData _ _ _ nt _)) ->
    dNewType nt >>>= \nt' ->
    showNT nt >>>= \ntstr ->
    showNT nt' >>>= \ntpstr ->
    dTrace ("Type syn  " ++ ntstr ++ " changed to " ++ ntpstr) $
{-    
    dCtxType noPos [] t >>>= \(_, t') ->
    showTheType t >>>= \st1 ->
    showTheType t' >>>= \st2 ->
    showSimple tid >>>= \ssimple ->
    dTrace ("Type syn:\n" ++ ssimple ++ " = " ++ st1 ++ "\nchanged to:\n" ++ ssimple ++ " = " ++ st2) $
-}
    updateSynType tid nt' >>>
    unitS [DeclIgnore "Type Synonym"]

dTopDecl (DeclData mb ctx simple constrs tycls) =
    dTrace ("DbgDataTrans.dTopDecl.DeclData") $
    unitS (:[])  =>>> (unitS (DeclData mb ctx) {- =>>> addCtx ctx simple-} =>>> unitS simple 
                 =>>> mapS dConstr constrs =>>> unitS tycls)
dTopDecl d@(DeclConstrs pos id constrids) = 
    dTrace ("DbgDataTrans.dTopDecl.DeclConstrs" ++ show constrids) $
    lookupName noPos id >>>= \(Just idinfo) ->
        case idinfo of
	     InfoData did tid ie nt dk ->
	         dTrace ("InfoData: " ++ show tid) $
	         case dk of
		     Data b constrs ->
		         mapS0 (addConstr pos) constrs >>>
		         mapS0 transformConstr constrs >>>
			 unitS [d]
		     DataNewType b constrs ->
		         mapS0 (addConstr pos) constrs >>>
		         mapS0 transformConstr constrs >>>
			 unitS [d]
		     _ -> error ("dk = " ++ show dk)
	     _ -> error ("idinfo = " ++ show idinfo)
    --lookupNameStr id >>>= \idstr ->
    --error ("dDecl: DeclConstrs " ++ show pos ++ " " ++ idstr ++ " " ++ show constrids ++ "\n" ++ show idinfo)
    where transformConstr constr =
              lookupName noPos constr >>>= \(Just info) ->
	          case info of
		      InfoConstr cid tid fix nt annot ty ->
		          dNewType nt >>>= \nt' ->
			  wrapRNewType nt' >>>= \nt'' ->
			  showNT nt'' >>>= \ntstr ->
		          dTrace ("InfoConstr: " ++ show tid ++ " has new type " ++ ntstr) $
			  updateConstrType constr nt''
		      _ -> error ("info = " ++ show info)     
dTopDecl d@(DeclClass pos ctx id1 id2 decls) =
    -- Try to get class method types
    lookupName noPos id1 >>>= \clsinfo@(Just (InfoClass i tid ie nt ms ds at)) ->
    setClassVar id2 >=>
    mapS transformMethodType (zip ms ds) >>>= \hol ->    
    dNewType nt >>>= \nt' ->
    showNT nt' >>>= \ntstr ->
    dTrace ("*** Class type: " ++ ntstr ++ " ho: " ++ show (or hol)) $
    updateClassType i tid ie nt' ms ds at >>>
    dDecls decls >>>= \decls' ->
    unitS [DeclClass pos ctx id1 id2 decls']
    where transformMethodType (m, d) =
              lookupName noPos m >>>= \(Just (InfoMethod im _ _ _ _ _)) ->
              lookupName noPos d >>>= \(Just (InfoDMethod id tid nt' (Just arity) _)) ->
	          lookupNameStr id >>>= \mstr ->
		  if doTransform tid then
		      showNT nt' >>>= \ntstr' ->
		      dTrace ("Type for " ++ mstr ++ " : " ++ ntstr') $
		      dMethodNewType (arity == 0) nt' >>>= \nt'' -> 
		      updateMethodType im id nt'' >>>
		      lookupName noPos m >>>= \(Just (InfoMethod _ _ _ nt''' _ _)) ->
		      showNT nt''' >>>= \ntstr'' ->
		      dTrace ("Transformed into " ++ ntstr'') $
		      unitS (isHigherOrder id2 nt')
		   else
		      unitS (isHigherOrder id2 nt')
	  -- Ignore method starting with '_'
	  doTransform = ('_'/=) . last . unpackPS . extractV

dTopDecl (DeclInstance pos ctx id inst decls) = 
    dCtxType pos ctx inst >>>= \(_, inst') -> -- Don't change the context!!!
    dDecls decls >>>= \decls' ->
    unitS [DeclInstance pos ctx id inst' decls']
dTopDecl (DeclDataPrim pos id size) = 
    lookupNameStr id >>>= \idstr ->
    error ("Cannot yet deal with primitive datatypes (" ++ idstr ++ ", size=" ++ show size ++ ")")
dTopDecl d = dDecl d

dDecls (DeclsParse ds) = 
    getArities ds >=>
    unitS DeclsParse =>>> (mapS dDecl ds >>>= \dss -> unitS (concat dss))

dDecl d@(DeclDefault tys) = unitS [d]
dDecl d@(DeclVarsType vars ctx ty) = 
    mapS lookupNameStr (map snd vars) >>>= \ids ->
    if "main" `elem` ids then
        unitS [d]  -- Don't change the type of 'main'
    else
        dCtxType noPos ctx ty >>>= \(ctx', ty') ->
	wrapRT noPos ty' >>>= \ty'' ->
        showVarsType d >>>= \svt1 ->
        showVarsType (DeclVarsType vars ctx' ty'') >>>= \svt2 ->
	let checkForCAF (pos, id) =
	        getArity id >>>= \arity ->
	        if False {-arity == 0-} then
		    showVarsType (DeclVarsType vars ctx' ty'') >>>= \svt2 ->
                    dTrace ("Signature:\n" ++ svt1 ++ "\nchanged to:\n" ++ svt2) $
                    unitS (DeclVarsType [(pos, id)] ctx' ty'')
	        else
	            addD ty'' >>>= \ty''' -> 
		    addSR ty''' >>>= \ty'''' ->
		    showVarsType (DeclVarsType vars ctx' ty'''') >>>= \svt2 ->
                    dTrace ("Signature:\n" ++ svt1 ++ "\nchanged to:\n" ++ svt2) $
	            unitS (DeclVarsType [(pos, id)] ctx' ty'''')
	in  mapS checkForCAF vars
dDecl (DeclPat (Alt pat gdes decls)) = 
    unitS ((:[]) . DeclPat) =>>> (unitS (Alt pat) =>>> mapS dGdEs gdes =>>> dDecls decls)
dDecl d@(DeclFun pos id fundefs) = 
    unitS ((:[]) . DeclFun pos id) =>>> mapS dFunClause fundefs
dDecl d@(DeclIgnore _) = unitS [d]
dDecl d@(DeclError _) = unitS [d]
dDecl d@(DeclAnnot _ _) = unitS [d]
dDecl d@(DeclFixity _) = unitS [d]
dDecl d@(DeclPrimitive pos id i ty1) =
    lookupNameStr id >>>= \idstr ->
    -- A hack to be able to have primitives with untransformed types
    if all (uncurry (==)) (zip "._tprim_" (snd (break ('.'==) idstr))) then
	unitS [d]
    else
        dType ty1 >>>= \ty2 ->    
	wrapRT pos ty2 >>>= \ty3 ->
	addD ty3 >>>= \ty4 -> 
	addSR ty4 >>>= \ty5 ->
	unitS [DeclPrimitive pos id 2 ty5]
--    lookupNameStr id >>>= \idstr ->
--    error ("dDecl: DeclPrimitive " ++ show pos ++ " " ++ idstr ++ " " ++ show i)
dDecl x = error "Hmmm. No match in dbgDataTrans.dDecl"

dFunClause (Fun ps gdses decls) = 
    unitS (Fun ps) =>>> mapS dGdEs gdses =>>> dDecls decls

dGdEs (gd, e) = unitS pair =>>> dExp gd =>>> dExp e

dExps es = mapS dExp es

dExp (ExpLambda pos pats e)      = unitS (ExpLambda pos pats) =>>> dExp e
dExp (ExpLet pos decls e)        = unitS (ExpLet pos) =>>> dDecls decls =>>> dExp e
dExp (ExpCase pos e alts)        = unitS (ExpCase pos) =>>> dExp e =>>> mapS dAlt alts
dExp (ExpIf pos c e1 e2)         = unitS (ExpIf pos) =>>> dExp c =>>> dExp e1 =>>> dExp e2
dExp (ExpType pos e ctx t)       =   
    dCtxType pos ctx t >>>= \(ctx', t') ->
    wrapRT pos t' >>>= \t'' ->
    dExp e >>>= \e' ->
    showTheType t'' >>>= \st ->
    --trace (show e' ++ " has type " ++ show ctx' ++ " => " ++ st) $
    unitS (ExpType pos e' ctx' t'')
dExp (ExpApplication pos es)     = unitS (ExpApplication pos) =>>> dExps es
dExp (ExpList pos es)            = unitS (ExpList pos) =>>> dExps es
dExp (ExpVar pos id)             = unitS (ExpVar pos id)
dExp (ExpScc s e)                = error "ExpScc not supported when debugging"
dExp (ExpDo pos stmts)           = dRemoveDo pos stmts
dExp (ExpFatbar _ _)             = error "ExpFatbar not supported when debugging"
dExp ExpFail                     = error "ExpFail not supported when debugging"
dExp (ExpRecord _ _)             = error "ExpRecord not supported when debugging"
dExp (ExpInfixList _ _)          = error "ExpInfixList not supported when debugging"
dExp (ExpVarOp _ _)              = error "ExpVarOp not supported when debugging"
dExp (ExpConOp _ _)              = error "ExpConOp not supported when debugging"
dExp e                           = unitS e


dRemoveDo p [StmtExp exp] = dExp exp
dRemoveDo p (StmtExp exp:r) =
  let pos = getPos exp
  in 
    lookupId Var t_gtgt >>>= \ gtgt ->
    dExp exp >>>= \exp' ->
    dRemoveDo p r >>>= \ exp2 ->
    unitS (ExpApplication pos [ExpVar pos gtgt, exp', exp2])
dRemoveDo p (StmtLet decls :r) =
  let pos = getPos decls
  in 
    dDecls decls >>>= \decls' ->
    dRemoveDo p r >>>= \ exp2 ->
    unitS (ExpLet pos decls' exp2)
dRemoveDo p (StmtBind pat exp:r) =
  lookupId Var t_gtgteq >>>= \ gtgteq ->
  getState >>>= \ state ->
  dExp exp >>>= \exp' ->
  dRemoveDo p r >>>= \ exp2 ->
  let pos = getPos exp'
  in
    if nofail state pat
    then  
      unitS (ExpApplication pos [ExpVar pos gtgteq, exp', ExpLambda pos [pat] exp2])
    else
      lookupId Var t_zero >>>= \ zero ->
      lookupId Con tTrue >>>= \ true ->
      newVar pos >>>= \ x ->
      let eTrue = ExpCon pos true
      in unitS (ExpApplication pos [ExpVar pos gtgteq
				   ,exp'
				   ,ExpLambda pos [x] (ExpCase pos x [Alt pat               [(eTrue,exp2)]            (DeclsScc [])
								     ,Alt (PatWildcard pos) [(eTrue,ExpVar pos zero)] (DeclsScc [])
								    ])])

nofail state (ExpCon pos con) =
  case lookupIS state con of
    Just (InfoConstr unique tid fix nt fields iType) ->
      case lookupIS state iType of
	Just (InfoData   unique tid exp nt dk) ->
	  case dk of
	    (DataNewType unboxed constructors) -> True
	    (Data unboxed  constrs) -> length constrs == 1
nofail state (ExpVar _ _) = True
nofail state (ExpApplication pos es) = all (nofail state) es
nofail state (PatWildcard _) = True
nofail state (PatAs _ _ pat) = nofail state pat
nofail state (PatIrrefutable pos pat) = True
nofail state _ = False

dAlt (Alt pat gdexps decls) = 
    unitS (Alt pat) =>>> mapS dGdEs gdexps =>>> dDecls decls

-- Type translating functions

-- Translate a type. All entities are wrapped in the RT type

dType t =
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TSyn tTrace >>>= \trail ->
    let dt (TypeCons pos id ts) = 
            mapS dt ts >>>= \ts' ->
	    if id == arrow then
	         mapS (wrapRT pos) ts' >>>= \ts'' ->
	         unitS (TypeCons pos arrow [TypeCons pos trail [], TypeCons pos id ts''])
	     else
	         unitS (TypeCons pos id ts')
        dt (TypeApp t1 t2) = 
	    unitS TypeApp =>>> dt t1 =>>> dt t2
        dt (TypeStrict pos t2) = error "not yet (dType: TypeStrict)"
	dt t@(TypeVar pos id) =
           unitS t
    in  dt t

wrapRTtvars pos =
    lookupId TCon tR >>>= \rid ->
    unitS (\(t, tvs) -> (TypeCons pos rid [t], tvs))

wrapRT pos t =
    lookupId TCon tR >>>= \rid ->
    unitS (TypeCons pos rid [t])

dCtxType pos ctx ty' =
    dType ty' >>>= \ty'' ->
    unitS (ctx, ty'')

dMethodNewType isCaf (NewType free exist ctxs [nt]) =
--Y    lookupId TClass tDisplayable >>>= \dispid ->
    lookupId TClass tNum >>>= \numid ->
    mapS (\(c, v) -> isNumSubClass c >>>= \b -> unitS (if b then [v] else [])) ctxs >>>= \vss ->
    let newctxs = [(numid, v) | v <- snub (concat vss)] 
--Y                  ++
--Y                  [(dispid, tid) | tid <- tail free, tid `notElem` map snd ctxs] 
    in
    dNT nt >>>= \nt' ->
    (if False {-isCaf-} then wrapNTRT else topLevelNT) nt' >>>= \nt'' ->
    unitS  (NewType free exist (ctxs ++ newctxs) [nt''])   

dNewType (NewType free exist ctxs nts) =
    mapS dNT nts >>>= \nts' ->
    unitS  (NewType free exist ctxs nts')

dNT t =
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tTrace >>>= \trail ->
    lookupId TCon tR >>>= \rt ->
    lookupId TCon t_List >>>= \bilist ->
    lookupId TCon tList >>>= \list ->
    lookupId TSyn tString >>>= \string ->
    lookupId TCon tRString >>>= \rstring ->
    let dt (NTcons id ts) = 
            mapS dt ts >>>= \ts' ->
	    if id == arrow then
		unitS (NTcons arrow [NTcons trail [], NTcons id (wrapNTs ts')])
	    else 
	        lookupName noPos id >>>= \(Just info) ->
	            unitS (NTcons id ts')
        dt (NTapp t1 t2) = unitS NTapp =>>> dt t1 =>>> dt t2
        dt (NTstrict t) = unitS NTstrict =>>> dt t
	dt t@(NTvar id) = unitS t
	dt t@(NTany id) = unitS t
	isTuple (TupleId _) = True
	isTuple  _ = False
	wrapNTs = map (\nt -> NTcons rt [nt])
    in  dt t 

wrapNTRT nt =
    lookupId TCon tR >>>= \rt ->
    unitS (NTcons rt [nt])

wrapRNewType (NewType free exist ctxs ts) =
    let (t:rts) = reverse ts in
    lookupId TCon tR >>>= \rt ->
    unitS (NewType free exist ctxs (reverse (map (\t -> NTcons rt [t]) rts) ++ [t]))

topLevelNT nt =
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tSR >>>= \sr ->
    lookupId TCon tTrace >>>= \d ->
    wrapNTRT nt >>>= \nt' ->
    unitS (NTcons arrow [NTcons sr [], NTcons arrow [NTcons d [], nt']])

[] \\ ys = []
(x:xs) \\ ys = x : (xs \\ filter (x /=) ys)

dConstr (Constr pos id ts) = 
    lookupId TCon tR >>>= \rid ->
    unitS (Constr pos id) =>>> 
    unitS (map (\(a, b) -> (a, TypeCons pos rid [b])) ts)

addSR t = 
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tSR >>>= \sr ->
    unitS (tc arrow [tc sr [], t])

addD t =
    lookupId TCon tTrace >>>= \did ->
    lookupId TCon t_Arrow >>>= \arrow ->
    unitS (tc arrow [tc did [], t])

remR (TypeCons pos id [t]) =
    -- Remove RT wrapper on the top level in type synonyms
    unitS t

isHigherOrder cvar (NewType free exist ctxs ts) = trace ("+++ " ++ concat (map snt ts)) $ or (map (isHO False) ts)
    where isHO nc (NTcons id ts) = or (map (isHO False) ts)
          isHO nc (NTapp t1 t2) = isHO True t1 || isHO False t2
          isHO nc (NTstrict t) = isHO nc t
	  isHO nc (NTvar id) = False
	  isHO nc (NTany id) = id == 1 && nc
	  snt (NTcons id ts) = "(NTcons " ++ show id ++ concat [' ' : snt t | t <- ts] ++ ")"
          snt (NTapp t1 t2) = "(NTapp " ++ snt t1 ++ " " ++ snt t2 ++ ")"
          snt (NTstrict t) = "(NTstrict " ++ snt t ++ ")"
	  snt (NTvar id) = "(NTvar " ++ show id ++ ")"
	  snt (NTany id) = "(NTany " ++ show id ++ ")"

-- Utility functions

lookupId kind ident = \(Inherited lookupPrel _ _ _ _) s -> (lookupPrel (ident, kind), s)
lookupName pos ident = \inh s@(Threaded state _ _) -> (lookupIS state ident, s)
lookupNameStr ident  = \inh s@(Threaded state _ _) -> (strIS state ident, s)

-- Used for debugging
showTheType t     = \inh s@(Threaded state _ _) -> (strType False state t, s)
showContext ctxs  = \inh s@(Threaded state _ _) -> (strContexts False state ctxs, s)
showVarsType vt   = \inh s@(Threaded state _ _) -> (strVarsType False state vt, s)
showSimple simple = \inh s@(Threaded state _ _) -> (strSimple False state simple, s)
showNT (NewType free exist ctxs nts)  = \(Inherited lookupPrel _ cv reptree ot) s@(Threaded state _ _) -> 
    (niceCtxs Nothing state al ctxs ++ mixSpace (map (niceNT Nothing state al) nts), s)
    where al = arg ++ zip (map snd ctxs) (map (('_':).(:[])) ['a'..'z']) -- a-z is to short!
          arg = mkAL free

getArity id = \(Inherited _ alist _ _ _) s ->
    (assocDef alist (-1) id, s)
--    (assocDef alist (error ("Internal error: Can't find arity for id #" ++ show id)) id, s)

getArities ds = \(Inherited lookupPrel _ cv reptree ot) s ->
    let ga (DeclFun pos id (Fun pat _ _:_)) = [(id, length pat)]
        ga d = []
    in  (Inherited lookupPrel (concat (map ga ds)) cv reptree ot, s)

updateMethodType im id nt = \inh (Threaded (IntState unique rps st errors) d constrs) ->
    case lookupAT st id of
        Just (InfoDMethod u tid _ annots cls) ->
	    let st' = updateAT st id (\_ -> InfoDMethod u tid nt annots cls) in
	    case lookupAT st im of
	        Just (InfoMethod u tid fix _ annots cls) ->
		    let st'' = updateAT st' im (\_ -> InfoMethod u tid fix nt annots cls) in
		    Threaded (IntState unique rps st'' errors) d constrs

updateClassType i tid ie nt ms ds at = \inh (Threaded (IntState unique rps st errors) d constrs) ->
    let st' = updateAT st i (\_ -> InfoClass i tid ie nt ms ds at) in
    Threaded (IntState unique rps st' errors) d constrs

updateSynType tid nt = \inh (Threaded (IntState unique rps st errors) d constrs) ->
    case lookupAT st tid of
        Just (InfoData u rtid ie _ k) ->
	    let st' = updateAT st tid (\_ -> InfoData u rtid ie nt k) in
	    Threaded (IntState unique rps st' errors) d constrs

updateConstrType id nt = \inh (Threaded (IntState unique rps st errors) d constrs) ->
    case lookupAT st id of
        Just (InfoConstr cid tid fix _ annot ty) ->
	    let st' = updateAT st id (\_ -> InfoConstr cid tid fix nt annot ty) in
	    Threaded (IntState unique rps st' errors) d constrs

addConstr pos id = \inh (Threaded is d constrs) ->
    Threaded is d ((pos, id):constrs)

newVar pos = \_ (Threaded istate d cs) ->
                 case uniqueIS istate of
	             (i, is') -> (ExpVar pos i, Threaded is' d cs)

getState = \_ t@(Threaded is _ _) -> (is, t)

setClassVar id = \(Inherited lookupPrel alist _ reptree ot) s ->
    (Inherited lookupPrel alist id reptree ot, s)

getClassVar = \(Inherited _ _ cv _ _) s -> (cv, s)

isNumSubClass c = \(Inherited lookupPrel _ _ _ _) s@(Threaded is _ _) ->
    let dcnum  = lookupPrel (tDNum, TClass)
	scof c = case lookupIS is c of
	             Just info -> any (dcnum==) sc || any scof sc
		         where sc = superclassesI info
    in (scof c, s)

dTrace str c = \i@(Inherited _ _ cv _ ot) s -> (if ot then trace str else id) (c i s)

tc c ts = TypeCons noPos c ts

nubEq p [] = []
nubEq p (x:xs) = x : nubEq p (filter ((p x /=) . p) xs)
