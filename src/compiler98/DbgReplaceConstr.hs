{- ---------------------------------------------------------------------------
This file is not used at all!!!
-}
module DbgDataTrans(dbgDataTrans) where

import Tree234
import Extra(trace)
import IdKind
import TokenId
import DbgId
import IntState
import Syntax
import SyntaxPos
import Flags
import Bind
import RenameLib
import State
import NT
import Nice(niceCtxs, niceNT, mkAL)
import AssocTree
import Char 
import PackedString(PackedString, unpackPS, packString)

data Inherited = Inherited ((TokenId, IdKind) -> Int) [(Int, Int)] Int (Tree (Int, Int)) Bool
data Threaded = Threaded IntState [(Int,[(Pos,Int)])]

dbgDataTrans flags state reptree lookupPrel derived dptopdecls =
  if (sDbgTrans flags) then
      --trace (show derived) $
      case dTopDecls dptopdecls
		(Inherited lookupPrel [] 0 reptree (sTraceFns flags)) (Threaded state derived) of
          (decls', Threaded state' derived') -> (decls', derived', state')
  else
      (dptopdecls, derived, state)

dTopDecls (DeclsParse ds) = unitS DeclsParse =>>> mapS dTopDecl ds

dTopDecl d@(DeclClass pos ctx id1 id2 fundeps decls) =
    unitS (DeclClass pos ctx id1 id2 fundeps) =>>> dDecls decls
dTopDecl (DeclInstance pos ctx id insts decls) = 
    unitS (DeclInstance pos ctx id insts) =>>> dDecls decls
dTopDecl d = dDecl d

dDecls (DeclsParse ds) = unitS DeclsParse =>>> mapS dDecl ds

dDecl (DeclPat (Alt pat gdes decls)) = 
    unitS DeclPat =>>> (unitS (Alt pat) =>>> mapS dGdEs gdes =>>> dDecls decls)
dDecl (DeclFun pos id fundefs) = 
    unitS (DeclFun pos id) =>>> mapS dFunClause fundefs
dDecl d = unitS d

dFunClause (Fun ps gdses decls) = 
    unitS (Fun ps) =>>> mapS dGdEs gdses =>>> dDecls decls

dGdEs (gd, e) = unitS pair =>>> dExp gd =>>> dExp e

dExps es = mapS dExp es

dExp (ExpLambda pos pats e)      = unitS (ExpLambda pos pats) =>>> dExp e
dExp (ExpLet pos decls e)        = unitS (ExpLet pos) =>>> dDecls decls =>>> dExp e
dExp (ExpCase pos e alts)        = unitS (ExpCase pos) =>>> dExp e =>>> mapS dAlt alts
dExp (ExpIf pos c e1 e2)         = unitS (ExpIf pos) =>>> dExp c =>>> dExp e1 =>>> dExp e2
dExp (ExpType pos e ctx t)       = unitS (ExpType pos) =>>> dExp e =>>> unitS ctx =>>> unitS t 
dExp (ExpApplication pos [ExpCon pos id,)     = unitS (ExpApplication pos) =>>> dExps es
dExp (ExpApplication pos es)     = unitS (ExpApplication pos) =>>> dExps es
dExp (ExpList pos es)            = unitS (ExpList pos) =>>> dExps es
dExp (ExpVar pos id)             = unitS (ExpVar pos id) -- unitS (ExpVar pos) =>>> replacePreludeId id
dExp (ExpScc s e)                = error "ExpScc not supported when debugging"
dExp (ExpDo _ _)                 = error "ExpDo not supported when debugging"
dExp (ExpFatbar _ _)             = error "ExpFatbar not supported when debugging"
dExp ExpFail                     = error "ExpFail not supported when debugging"
dExp (ExpRecord _ _)             = error "ExpRecord not supported when debugging"
dExp (ExpInfixList _ _)          = error "ExpInfixList not supported when debugging"
dExp (ExpVarOp _ _)              = error "ExpVarOp not supported when debugging"
dExp (ExpConOp _ _)              = error "ExpConOp not supported when debugging"
dExp e                           = unitS e

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

dCtxType pos ctx ty =
    replaceListType ty >>>= \ty' ->   -- Don't do this when debugged list is built-in
--Z    unitS ty >>>= \ty' -> 
    dType ty' >>>= \ty'' ->
--Y    getClassVar >>>= \cv ->
--Y    lookupId TClass tDisplayable >>>= \dispid ->
    unitS (ctx {-++ [Context pos dispid (p, id) | (p, id) <- nubEq snd tvs, id `notElem` (cv:map tyvOf ctx)]-}, ty'') --Y
--Y    where tyvOf (Context _ _ (_, tv)) = tv

{--Y
addCtx ctx (Simple pos _ tvars) = 
    lookupId TClass tDisplayable >>>= \dispid ->
    unitS (ctx ++ [Context pos dispid pid | pid <- tvars])
-}

dMethodNewType isCaf (NewType all exist ctxs [nt]) =
--Y    lookupId TClass tDisplayable >>>= \dispid ->
    lookupId TClass tNum >>>= \numid ->
    mapS (\(c, v) -> isNumSubClass c >>>= \b -> unitS (if b then [v] else [])) ctxs >>>= \vss ->
    let newctxs = [(numid, v) | v <- snub (concat vss)] 
--Y                  ++
--Y                  [(dispid, tid) | tid <- tail (all+exit), tid `notElem` map snd ctxs] 
    in
    dNT nt >>>= \nt' ->
    (if False {-isCaf-} then wrapNTRT else topLevelNT) nt' >>>= \nt'' ->
    unitS  (NewType all exist (ctxs ++ newctxs) [nt''])   

dClassType isho clsvar (NewType all exist ctxs nts) =
--Y    lookupId TClass tDisplayable >>>= \dispid ->
--Y    let ctxsvars = (if isho then (clsvar:) else id) (map snd ctxs)
--Y        newctxs = [ (dispid, tid) | tid <- all++exist, tid `notElem` ctxsvars] in
    mapS dNT nts >>>= \nts' ->
    unitS  (NewType all exist (ctxs {- ++ newctxs-}) nts')   --Y

dNewType (NewType all exist ctxs nts) =
--Y    lookupId TClass tDisplayable >>>= \dispid ->
--Y    let newctxs = [ (dispid, tid) | tid <- all++exist, tid `notElem` map snd ctxs] in
    mapS dNT nts >>>= \nts' ->
    unitS  (NewType all exist (ctxs {- ++ newctxs-}) nts')   --Y

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
{-
	    else if id == bilist then      -- Replace built-in list with debugged list
	        unitS (NTcons list ts')
	    else if id == string then      -- Replace built-in string with debugged string
	        unitS (NTcons rstring ts')
-}
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

wrapRNewType (NewType all exist ctxs ts) =
    let (t:rts) = reverse ts in
    lookupId TCon tR >>>= \rt ->
    unitS (NewType all exist ctxs (reverse (map (\t -> NTcons rt [t]) rts) ++ [t]))

topLevelNT nt =
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tSR >>>= \sr ->
    lookupId TCon tTrace >>>= \d ->
    wrapNTRT nt >>>= \nt' ->
    unitS (NTcons arrow [NTcons sr [], NTcons arrow [NTcons d [], nt']])

[] \\ ys = []
(x:xs) \\ ys = x : (xs \\ filter (x /=) ys)

dConstr (Constr pos id ts) = 
    mapS replaceListType (map snd ts) >>>= \ts' ->    -- Don't do this when debugged list is built-in
--Z    unitS (map snd ts) >>>= \ts' ->    -- Don't do this when debugged list is built-in
    lookupId TCon tR >>>= \rid ->
    unitS (Constr pos id) =>>> unitS (map (\(a, b) -> (a, TypeCons pos rid [b])) (zip (map fst ts) ts'))

replaceListType t = unitS t -- Don't need this anymore :)
{-
    lookupId TCon t_List >>>= \bilist ->
    lookupId TCon tList >>>= \list ->
    lookupId TSyn tString >>>= \string ->
    lookupId TCon tRString >>>= \rstring ->
    let rlt (TypeCons pos id ts) =
            unitS (TypeCons pos) 
	          =>>> unitS (if id==bilist then list else if id==string then rstring else id) 
		  =>>> mapS rlt ts
        rlt (TypeApp t1 t2) = unitS TypeApp =>>> rlt t1 =>>> rlt t2
        rlt (TypeStrict pos t2) = error "not yet (replaceListType: TypeStrict)"
{-
	rlt (TypeTuple pos ts) =
	    unitS (TypeTuple pos) =>>> mapS rlt ts
-}
	rlt t = unitS t
    in  rlt t 
-}

enforceDeriving tycls = unitS tycls
{-
    lookupId TClass tShow >>>= \htid ->
    lookupId TClass tOrd >>>= \hoid ->
    lookupId TClass tEq >>>= \heid ->
    lookupId TClass tDisplayable >>>= \hdid ->
    unitS (tycls ++ map (pair noPos) (filter (not . (`elem` (map snd tycls)))
                                             [htid, hoid, heid, hdid]))
-}

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

isHigherOrder cvar (NewType all exist ctxs ts) = trace ("+++ " ++ concat (map snt ts)) $ or (map (isHO False) ts)
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
lookupName pos ident = \inh s@(Threaded state d) -> (lookupIS state ident, s)
lookupNameStr ident  = \inh s@(Threaded state d) -> (strIS state ident, s)

-- Used for debugging
showTheType t     = \inh s@(Threaded state d) -> (strType False state t, s)
showContext ctxs  = \inh s@(Threaded state d) -> (strContexts False state ctxs, s)
showVarsType vt   = \inh s@(Threaded state d) -> (strVarsType False state vt, s)
showSimple simple = \inh s@(Threaded state d) -> (strSimple False state simple, s)
showNT (NewType all exist ctxs nts)  = \(Inherited lookupPrel _ cv reptree ot) s@(Threaded state d) -> 
    (niceCtxs Nothing state al ctxs ++ mixSpace (map (niceNT Nothing state al) nts), s)
    where al = arg ++ zip (map snd ctxs) (map (('_':).(:[])) ['a'..'z']) -- a-z is to short!
          arg = mkAL (all ++ exist)
	  
--    (strNT (strIS state) strTVar nt, s)

strTVar v = let cv =  toEnum (v + fromEnum 'a')
            in if isAlpha cv
	       then [cv]
	       else '_':show v

getArity id = \(Inherited _ alist _ _ _) s ->
    (assocDef alist (-1) id, s)
--    (assocDef alist (error ("Internal error: Can't find arity for id #" ++ show id)) id, s)

getArities ds = \(Inherited lookupPrel _ cv reptree ot) s ->
    let ga (DeclFun pos id (Fun pat _ _:_)) = [(id, length pat)]
        ga d = []
    in  (Inherited lookupPrel (concat (map ga ds)) cv reptree ot, s)

updateMethodType im id nt = \inh (Threaded (IntState unique rps st errors) d) ->
    case lookupAT st id of
        Just (InfoDMethod u tid _ annots cls) ->
	    let st' = updateAT st id (\_ -> InfoDMethod u tid nt annots cls) in
	    case lookupAT st im of
	        Just (InfoMethod u tid ie fix _ annots cls) ->
		    let st'' = updateAT st' im (\_ -> InfoMethod u tid ie fix nt annots cls) in
		    Threaded (IntState unique rps st'' errors) d

updateClassType i tid ie nt ms ds at = \inh (Threaded (IntState unique rps st errors) d) ->
    let st' = updateAT st i (\_ -> InfoClass i tid ie nt ms ds at) in
    Threaded (IntState unique rps st' errors) d

updateSynType tid nt = \inh (Threaded (IntState unique rps st errors) d) ->
    case lookupAT st tid of
        Just (InfoData u rtid ie _ k) ->
	    let st' = updateAT st tid (\_ -> InfoData u rtid ie nt k) in
	    Threaded (IntState unique rps st' errors) d

updateConstrType id nt = \inh (Threaded (IntState unique rps st errors) d) ->
    case lookupAT st id of
        Just (InfoConstr cid tid ie fix _ annot ty) ->
	    let st' = updateAT st id (\_ -> InfoConstr cid tid ie fix nt annot ty) in
	    Threaded (IntState unique rps st' errors) d

replacePreludeId id = \(Inherited _ _ _ reptree _) s -> 
                          (treeSearch id snd (compare id . fst) reptree, s)

setClassVar id = \(Inherited lookupPrel alist _ reptree ot) s ->
    (Inherited lookupPrel alist id reptree ot, s)

getClassVar = \(Inherited _ _ cv _ _) s -> (cv, s)

isNumSubClass c = \(Inherited lookupPrel _ _ _ _) s@(Threaded is _) ->
    let dcnum  = lookupPrel (tDNum, TClass)
	scof c = case lookupIS is c of
	             Just info -> any (dcnum==) sc || any scof sc
		         where sc = superclassesI info
    in (scof c, s)

dTrace str c = \i@(Inherited _ _ cv _ ot) s -> (if ot then trace str else id) (c i s)

tc c ts = TypeCons noPos c ts

nubEq p [] = []
nubEq p (x:xs) = x : nubEq p (filter ((p x /=) . p) xs)
