{- ---------------------------------------------------------------------------
Transforms all value definitions of a program 
for producing traces for debugging.
-}
module DbgTrans(SRIDTable,debugTrans, dbgAddImport) where

import Extra(Pos, noPos, pair, fromPos, strPos, dropJust, trace)
import IdKind(IdKind(Con,Var))
import TokenId
import DbgId
import IntState(IntState(IntState),addIS,arityIS,arityVI,lookupIS,strIS
               ,uniqueIS,uniqueISs,IE(IEnone),ntI,updateIS
               ,Info(InfoVar,InfoClass,InfoMethod,InfoDMethod,InfoIMethod))
import Syntax
import SyntaxPos
import StrSyntax
import NT
import State
import AssocTree
import PackedString(PackedString, unpackPS, packString)
import Id(Id)


{- table for source references and identifiers refered to from the trace -}
type SRIDTable = Maybe ((Int,[Int])        -- source reference table
                       ,[(Pos,Id)]         -- identifier table
                       ,[ImpDecl TokenId]  -- import declarations
                       ,String)            -- module name


-- newNameVar, nameArity, hsFromInt, addInt

data Inherited = Inherited 
                   (Exp Id) 
                   (Exp Id) 
                   ((TokenId, IdKind) -> Id) -- lookupPrel

data Threaded = Threaded 
                  IntState 
                  (Id, [Id])   -- source reference table, 
                               -- accumulated for SRIDTable
                  [(Pos, Id)]  -- identifier table, accumulated for SRIDTable
                -- (AssocTree Int (Exp Int)) -- [(Int, Int)]  ??


type DbgTransMonad a = State Inherited Threaded a Threaded


{-
Used to add the special prelude for debugging to the import list.
Now it serves no purpose and is just the identity function.
-}
dbgAddImport :: Bool -> Module a -> Module a

dbgAddImport dodbg m@(Module pos id exports imports fixities decls) =
    Module pos id exports (dbgAI dodbg imports) fixities decls
    where dbgAI True impdecls = 
              {-ImportQ (noPos, tDbgPrelude) (Hiding []) :-} impdecls
{-
	  dbgAI True True impdecls = 
	      {-ImportQ (noPos, tDbgPreludeCore) (Hiding []) :-} impdecls
-}
	  dbgAI False impdecls = impdecls


{-
Transforms all value definitions for producing traces for debugging.
-}
debugTrans :: a 
           -> IntState 
           -> ((TokenId,IdKind) -> Id) 
           -> PackedString 
           -> b 
           -> [ImpDecl TokenId]  
           -> Decls Id 
           -> Maybe [(Pos,Id)] -- data constructors defined by data/newtype
           -> (Decls Id        -- transformed declarations
              ,IntState
              ,SRIDTable)

debugTrans flags istate lookupPrel modidl modid impdecls decls (Just constrs) =
  initDebugTranslate (dTopDecls decls) istate lookupPrel
  where 
  initDebugTranslate f istate lookupPrel = 
    case f (Inherited start_d fatal lookupPrel) 
           (Threaded istate (1, [0]) constrs) of
      (decls', Threaded istate' srt idt) -> 
        (decls', istate', Just (srt, idt, impdecls, reverse (unpackPS modidl)))
  start_d :: Exp Id
  start_d = ExpCon noPos (lookupPrel (t_Root, Con)) 
            -- data constructor Root of the R type 
  fatal :: Exp Id
  fatal = ExpCon noPos (lookupPrel (t_fatal, Var))
          -- variable ? fatal


dTopDecls :: Decls Int -> DbgTransMonad (Decls Int)

dTopDecls (DeclsParse ds) = 
    mapS dTopDecl ds >>>= \dss -> 
       unitS (DeclsParse (concat dss))


dTopDecl :: Decl Int -> DbgTransMonad [Decl Int]

dTopDecl d@(DeclType id t) = unitS [d]
dTopDecl d@(DeclData mb ctx id contrs tycls) = unitS [d]
dTopDecl d@(DeclConstrs pos id constrids) = unitS [d]
dTopDecl (DeclClass pos ctx id1 id2 decls) =
    dDecls decls >>>= \decls' ->
    lookupName id1 >>>= \(Just (InfoClass i tid ie nt ms ds at)) ->
    mapS0 fixMethodArity (zip ms ds) >>>
    unitS [DeclClass pos ctx id1 id2 decls']
    where fixMethodArity (m, d) =
              lookupName d >>>= \(Just (InfoDMethod _ _ _ (Just arity) _)) ->
	      setArity 2 {-arity-} m
dTopDecl d@(DeclInstance pos ctx id inst decls) = 
{-YYY
    lookupId TClass tDisplayable >>>= \hdid ->
    lookupId TClass tShow >>>= \htid ->
    lookupId TClass tEq >>>= \heid ->
    lookupId TClass tOrd >>>= \hoid ->
    lookupId TClass tEval >>>= \hevid ->
    if id `elem` [hdid, htid, heid, hoid, hevid] then
        unitS [d]
    else
-}
        unitS  ((:[]) . DeclInstance pos ctx id inst) =>>> dDecls decls
dTopDecl d = dDecl d


dDecls :: Decls Id -> DbgTransMonad (Decls Id)

dDecls (DeclsParse ds) = 
    mapS dDecl ds >>>= \dss -> 
    unitS (DeclsParse (concat dss))


dDecl :: Decl Id -> DbgTransMonad [Decl Id]

dDecl d@(DeclDefault tys) = unitS [d]
dDecl d@(DeclVarsType vars ctx ty) = unitS [d] 
dDecl (DeclPat (Alt pat [(g, e)] decls)) =
    addNewName 0 True "_pv" NoType >>>= \patid ->
    --trace ("patid = " ++ show patid) $
    setArity 0 patid >>>
    patVars pat >>>= \(pat', bvsnvs) ->
    let bvsids = map (snd . fst) bvsnvs in
    mapS0 (setArity 0) bvsids >>>
    mapS0 addId (map fst bvsnvs) >>>
    mapS makeSourceRef (map (fst . fst) bvsnvs) >>>= \srs ->
    dExp False e >>>= \e' ->
    dPat pat' >>>= \pat'' ->
    dDecls decls >>>= \decls' ->
    lookupId Var t_patvar >>>= \pvid ->
    lookupCon noPos tNTId >>>= \ntid ->
    getD >>>= \redex ->
    newVar noPos >>>= \fp ->
    mkFailExpr (fst (fst (head bvsnvs))) {-Wrong!-}{-("pattern")-} >>>= \fe ->
    noGuard >>>= \ng ->
    let evars = map snd bvsnvs in 
    makeTuple noPos evars >>>= \etup ->
    let pcase = ExpCase noPos e' [Alt pat'' [(g, etup)] (DeclsParse [])
	                         ,Alt fp    [(ng, fe)]  (DeclsParse [])]
	pfun = DeclFun noPos patid [Fun [] [(g, pcase)] decls']
{-
	vpat p pv i sr = ExpApplication p [ExpVar p pvid, 
	                                   ExpApplication p 
                                             [ntid 
					     ,ExpLit p (LitInt Boxed i)],
					   vcase p pv{-, sr, redex-}]
        vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup [(g, pv)] (DeclsParse [])]
        vfun ((p, i), pv) sr = DeclFun p i 
                                 [Fun [] [(g, vpat p pv i sr)] (DeclsParse [])]
-}
	vpat p pv i sr = vcase p pv
        vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup [(g, pv)] (DeclsParse [])]
        vfun ((p, i), pv) sr = DeclFun p i 
                                 [Fun [PatWildcard p, PatWildcard p] 
                                      [(g, vpat p pv i sr)] (DeclsParse [])]
    in unitS (pfun : zipWith vfun bvsnvs srs)
dDecl d@(DeclPat _) =
    error ("dDecl: pattern binding with guards")
dDecl d@(DeclFun pos id fundefs) = 
    addId (pos, id) >>>
    lookupName id >>>= \(Just info) ->
    getIdArity id >>>= \oa ->
    lookupNameStr id >>>= \funName ->
    --trace ("DeclFun: funName = " ++ funName ++ ", arity " ++ show info) $
    if isCMethod info then
      dMethod info pos id funName fundefs
    else
      let arity = getArity fundefs
      in case arity of
           0 -> dCaf pos id funName fundefs (unwrapNT 0 True (ntI info))
           _ -> dFun pos id funName arity fundefs 
                  (unwrapNT arity False (ntI info))
dDecl d@(DeclIgnore _) = unitS [d]
dDecl d@(DeclError _) = unitS [d]
dDecl d@(DeclAnnot _ _) = unitS [d]
dDecl d@(DeclFixity _) = unitS [d]
dDecl d@(DeclPrimitive pos id i ty) = unitS [d]
dDecl d@(DeclForeignImp pos cname id' arity cast typ id) =
    addId (pos, id) >>>
    lookupName id' >>>= \(Just info) ->
--  lookupNameStr id' >>>= \funName ->
--  trace ("DeclForeignImp: " ++ funName ++", info=\n" ++ show info) $
--  addNewWrapper info >>>= \id' ->
--  overwriteOrigName id' >>>
    -- generate code for the wrapper
    dPrim pos id id' arity >>>= \code->
    -- get real cname from primed hname (f'), if needed
    let (InfoVar _ (Qualified _ f) _ _ _ _) = info
        cname' = if null cname then reverse (tail (unpackPS f)) else cname
    in
    unitS [ DeclForeignImp pos cname' id' arity cast typ id
          , DeclFun pos id [code] ]
dDecl d@(DeclForeignExp pos cname id typ) =
	error ("Can't trace foreign exports yet. "++strPos pos)

dMethod info@(InfoDMethod _ tid nt (Just arity) _) pos id funName fundefs = 
    --trace ("InfoD: " ++show info) $
    if False {-not (doTransform tid)-} then
        unitS [DeclFun pos id fundefs]
--    else if arity == 0 then
--        dCaf pos id funName fundefs
    else if getArity fundefs == arity then
        dFun pos id funName arity fundefs NoType --(unwrapNT False nt)
    else
        case fundefs of
            [Fun [] [(g, ExpApplication p1 [ExpVar p2 te, emsg])] (DeclsParse [])] ->
	        lookupId Var t_error >>>= \errorid ->
		if te == errorid then
		    setArity 2 id >>>
	            lookupId Var t_fatal >>>= \fatal ->
		    newVars pos 2 >>>= \[sr, redex] ->
		    unitS [DeclFun pos id [Fun [sr, redex]  
		                          [(g, ExpApplication p1 [ExpVar p2 fatal, redex])]
					  (DeclsParse [])]]
		 else
		     dFun pos id funName arity fundefs NoType --(unwrapNT False nt)
	    _ -> dFun pos id funName (getArity fundefs) fundefs NoType --(unwrapNT False nt)
--	    _ -> dFun pos id funName arity fundefs NoType --(unwrapNT False nt)
dMethod info@(InfoIMethod _ tid nt (Just arity) _) pos id funName fundefs = 
    --trace ("InfoI: " ++show info) $
    if False {-not (doTransform tid)-} then
        unitS [DeclFun pos id fundefs]
    else -- if (getArity fundefs) {-arity-} == 0 then  --- Wrong !!!!
--        dCaf pos id funName fundefs
--    else 
        case fundefs of
           [Fun [] [(g, ExpVar p1 d)] (DeclsParse [])] ->
               -- This must(?) be a wrapper to the method of the superclass
               setArity 2 id >>>
	       newVars pos 2 >>>= \[sr, redex] ->
	       unitS [DeclFun pos id [Fun [sr, redex]
	                              [(g, ExpApplication p1 [ExpVar p1 d, sr, redex])]
				      (DeclsParse [])]]
	   _ -> dFun pos id funName (getArity fundefs) fundefs NoType--(unwrapNT False nt)



doTransform :: TokenId -> Bool

doTransform = ('_'/=) . last . unpackPS . extractV


dCaf :: Pos -> Id -> String -> [Fun Id] -> NewType -> DbgTransMonad [Decl Id]

dCaf pos id cafName fundefs nt =
    lookupCon pos tNTId >>>= \ntid ->
    lookupCon pos t_Nm >>>= \nm ->
    lookupCon pos tR >>>= \r ->
    lookupCon pos t_Sat >>>= \sat ->
    lookupVar pos t_trust >>>= \trust ->
    lookupVar pos t_cSeq >>>= \cSeq ->
    lookupCon pos tE >>>= \te ->
    makeSourceRef pos >>>= \sr ->
    setArity 2 id >>>
    addNewName 0 True "nt" NoType >>>= \t' ->
    addNewName 0 True "tr" NoType >>>= \tr ->
    addNewName 0 True cafName nt >>>= \nid ->
    getD >>>= \redex ->                  -- Use the surrounding redex
    setD (ExpVar pos t') >=>
    mapS dCafClause fundefs >>>= \[Fun [] [(_, e)] (DeclsParse decls)] ->
    newVar pos >>>= \v ->
    newVar pos >>>= \ot ->
    noGuard >>>= \ng ->
    let tre = ExpVar pos tr
	nte = {-ExpIf pos tre redex-} 
              (ExpApplication pos 
		 [nm, redex, ExpApplication pos 
                               [ntid, ExpLit pos (LitInt Boxed id)], sr]
              )
        ce = {-ExpApplication pos 
                 [cSeq, ExpVar pos t', (ExpApplication pos 
                           [te, ExpIf pos tre (ExpApplication pos [r, v, ot])-}
	     (ExpApplication pos [r, v, ExpApplication pos 
					  [sat, ExpVar pos t', ot]])
             {-])]-}
    in

  unitS [DeclFun pos id 
           [Fun [PatWildcard pos, PatWildcard pos] 
                [(ng, ExpVar pos nid)] (DeclsParse [])
           ]
        ,DeclFun pos nid 
           [Fun [] 
             [(ng, ExpCase pos e 
                     [Alt (PatIrrefutable pos (ExpApplication pos [r, v, ot]))
		  	  [(ng, ce)] (DeclsParse [])
                     ]
             )]
	     (DeclsParse
	        (DeclFun pos t' [Fun [] [(ng, nte)] (DeclsParse [])] 
                :DeclFun pos tr [Fun [] [(ng
                                         ,ExpApplication pos [trust, redex])]
		                   (DeclsParse [])
                                ]
                :decls
                )
             )
           ]
        ]


dFun :: Pos -> Id -> String -> Int -> [Fun Id] -> NewType
     -> DbgTransMonad [Decl Int]
   
dFun pos id funName arity fundefs nt =
    lookupVar pos (t_fun arity) >>>= \fun ->
    lookupCon pos tNTId >>>= \ntid ->
    setArity 2 id >>>
    newVar pos >>>= \redex ->
    newVar pos >>>= \sr ->
    -- buildFun pos id (arity+1) [redex] funName fun redex fundefs
    addNewName (arity+1) True funName nt >>>= \wrappedfun ->	    
    newVars pos (arity+1) >>>= \(newredex:fp) ->
    setD newredex >=>
    lookupId Con tTrue >>>= \true ->
    lookupId Var t_otherwise >>>= \otherwise ->
    checkPrimitive fundefs >>>= \prim ->
    (case prim of
        Nothing -> dFunClauses funName true otherwise fundefs
--        Nothing -> mapS dFunClause fundefs
        Just fundefs -> unitS (fundefs, [])
--        Just fundefs -> unitS (fundefs)
    ) >>>= \(fundefs', newdecls) ->
--    ) >>>= \(fundefs') ->
    mkFailExpr pos >>>= \fpexp ->
    noGuard >>>= \ng ->
    let fpclause = Fun (newredex:fp) [(ng, fpexp)] (DeclsParse [])
    in unitS ([DeclFun pos id [Fun [sr, redex]
                                  [(ng, 
                                    ExpApplication pos 
				        [fun,
					 ExpApplication pos 
					     [ntid, ExpLit pos (LitInt Boxed id)],
					 --eStr pos (stripPrelude funName),
					 ExpVar pos wrappedfun, sr, redex])]
				  (DeclsParse [DeclFun pos wrappedfun (fundefs'++[fpclause])])]] ++ newdecls)
--				  (DeclsParse [DeclFun pos wrappedfun (fundefs'++[fpclause])])]])


{-
For each clause of the function definition e, return a transformed
definition e', and possibly declare a new auxiliary function to handle
failure across guards.
-}
dFunClauses :: String -> Id -> Id -> [Fun Id] 
            -> DbgTransMonad ([Fun Int],[Decl Int])

dFunClauses funName true otherwise [] = unitS ([], [])
-- No guards (i.e. guard==True) is the easiest case.
--     f pat1 pat2 ... = e  where decls
-- ==>
--     f t pat1' pat2' ... = e' where decls'
dFunClauses funName true otherwise 
  (Fun pats [(gd@(ExpCon p cid), e)] decls:fcs)
    | true == cid = 
       getD >>>= \t ->
       dPats pats >>>= \pats' ->
       dExp False e >>>= \e' ->
       dDecls decls >>>= \decls' ->
       dFunClauses funName true otherwise fcs >>>= \(mfs, nfs) ->
       unitS (Fun (t:pats') [(gd, e')] decls':mfs, nfs)
-- For the final clause containing > 1 guard:
dFunClauses funName true otherwise [Fun pats ges decls] =
    normalFail >=>
    if canFail true otherwise ges then
       mapS namePat pats >>>= \namedpats ->
       let (patnames, pats') = unzip namedpats in
       dPats pats' >>>= \pats'' ->
       newVar noPos >>>= \t ->
       setD t >=>
       dGuardedExprs ges >>>= \expr ->
       dDecls decls >>>= \decls' ->
       getFail >>>= \fail ->
       let failclause = 
            Fun (t:patnames) [(ExpCon noPos true, addAppTrace fail t)] 
	        (DeclsParse []) in
       unitS ([Fun (t:pats'') [(ExpCon noPos true, ExpApplication noPos [expr, t])] decls', failclause], [])
    else
       getD >>>= \t ->
       dPats pats >>>= \pats' ->
       dGuardedExprs ges >>>= \e ->
       dDecls decls >>>= \decls' ->
       let fs = [Fun (t:pats') [(ExpCon noPos true, 
	                         ExpApplication noPos [e, t])] decls'] in
       unitS (fs, [])
-- For non-final clauses containing > 1 guard:
dFunClauses funName true otherwise (Fun pats ges decls:fcs) =
    if canFail true otherwise ges then
       mapS namePat pats >>>= \namedpats ->
       let (patnames, pats') = unzip namedpats in
       addNewName (length pats' + 1) True funName NoType >>>= \f ->
       let fail = ExpApplication noPos (ExpVar noPos f:patnames) in
       setFail fail >=>
       dPats pats' >>>= \pats'' ->
       newVar noPos >>>= \t ->
       setD t >=>
       dGuardedExprs ges >>>= \expr ->
       dDecls decls >>>= \decls' ->
       let failclause = Fun (t:patnames) [(ExpCon noPos true, addAppTrace fail t)] (DeclsParse []) in
       dFunClauses funName true otherwise fcs >>>= \(mfs, nfs) ->
       unitS ([Fun (t:pats'') [(ExpCon noPos true, ExpApplication noPos [expr, t])] decls', failclause], DeclFun noPos f mfs:nfs)
    else
       normalFail >=>
       getD >>>= \t ->
       dPats pats >>>= \pats' ->
       dGuardedExprs ges >>>= \e ->
       dDecls decls >>>= \decls' ->
       dFunClauses funName true otherwise fcs >>>= \(mfs, nfs) ->
       let fs = Fun (t:pats') [(ExpCon noPos true, ExpApplication noPos [e, t])] decls' in
       unitS (fs:mfs, nfs)

{-
-- id is the new function we are declaring; id' is the real foreign function
-}
dPrim :: Pos -> Id -> Id -> Int -> DbgTransMonad (Fun Id)

dPrim pos id id' arity =
    lookupVar pos (t_primn arity) >>>= \primn ->
    lookupCon pos tNTId >>>= \ntid ->
    setArity 2 id >>>
    newVar pos >>>= \redex ->
    newVar pos >>>= \sr ->
    noGuard >>>= \ng ->
    unitS (Fun [sr, redex]
               [( ng
                , ExpApplication pos 
                    [ primn
                    , ExpApplication pos [ntid, ExpLit pos (LitInt Boxed id)]
                    , ExpVar pos id', sr, redex])]
               (DeclsParse []))

dGuardedExprs [] = 
    getFail >>>= \fail ->
    newVar noPos >>>= \t ->
    unitS (ExpLambda noPos [t] (addAppTrace fail t))
dGuardedExprs ((g, e):ges) = 
    let pos = getPos g in
    dGuardedExprs ges >>>= \ges' ->
    dExp False g >>>= \g' ->
    newVar pos >>>= \t ->
    setD t >=>
    dExp False e >>>= \e' ->
    lookupVar pos t_guard >>>= \guard ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [guard, sr, g', ExpLambda pos [t] e', ges'])

addAppTrace (ExpApplication pos (f:es)) t = ExpApplication pos (f:t:es)
addAppTrace f t = ExpApplication noPos [f, t]

namePat e@(ExpVar p v) = unitS (e, e)
namePat e@(PatAs p v pat) = unitS (ExpVar p v, e)
namePat pat = 
    newVar noPos >>>= \e@(ExpVar _ v) -> unitS (e, PatAs noPos v pat)


{-
Returns False only if the one of the guards definitely has value True.
-}
canFail :: Id  -- of constructor True
        -> Id  -- of variable otherwise
        -> [(Exp Id, Exp Id)] -- guarded expressions
        -> Bool

canFail true otherwise [] = True
canFail true otherwise ((ExpCon _ cid, _):gdes) = 
    (true /= cid) && canFail true otherwise gdes
canFail true otherwise ((ExpVar _ cid, _):gdes) = 
    (otherwise /= cid) && canFail true otherwise gdes
canFail true otherwise (_:gdes) = canFail true otherwise gdes

checkPrimitive [Fun ps [(gd, ExpApplication pos (ExpVar p id:f:es))] decls] =
    lookupId Var t_prim >>>= \primid ->
    if id == primid then
        getD >>>= \redex ->
	lookupVar pos t_rseq >>>= \rseq ->
--	let expr = ExpApplication pos (f:redex:es)
	let expr = foldr (\n e -> ExpApplication pos [rseq, n, e]) 
	                 (ExpApplication pos (f:redex:es))
			 es
        in unitS (Just [Fun (redex:ps) [(gd, expr)] decls])
    else
        unitS Nothing
checkPrimitive _ = unitS Nothing

mkFailExpr pos =
    lookupVar pos t_fatal >>>= \fatal ->
    getD >>>= \redex ->
    unitS (ExpApplication pos [fatal, redex])

dCafClause (Fun ps gdses decls) = 
    unitS Fun =>>> dPats ps =>>> mapS dGdEs gdses =>>> dDecls decls -- ps should be empty

dFunClause (Fun ps gdses decls) = 
    unitS Fun =>>> (unitS (:) =>>> getD =>>> dPats ps) =>>> mapS dGdEs gdses =>>> dDecls decls

dGdEs (gd, e) = unitS pair =>>> dGuard gd =>>> dExp False e

dExps cr es = mapS (dExp cr) es


{-

-}
dExp :: Bool -> Exp Id -> DbgTransMonad (Exp Id)

dExp cr (ExpLambda pos pats e) = 
    newVar pos >>>= \redex ->
    newVars pos (length pats) >>>= \npats ->
    getD >>>= \oldredex ->
    setD redex >=>
    dExp False e >>>= \e' ->
    lookupVar pos (t_fun (length pats)) >>>= \fun ->
    lookupCon pos tLambda >>>= \lambda ->
    --getModStr >>>= \modstr ->
    noGuard >>>= \ng ->
    dPats pats >>>= \pats' ->
    newVar pos >>>= \fpat ->
    mkFailExpr pos {-Wrong!-} >>>= \fpexp ->
    makeSourceRef pos >>>= \sr ->
    makeTuple pos npats >>>= \npatstup ->
    makeTuple pos pats' >>>= \patstup ->
    let lamexp = ExpLambda pos (redex:npats) (ExpCase pos npatstup alts)
	alts = [Alt patstup [(ng, e')] (DeclsParse []),
	        Alt fpat [(ng, fpexp)] (DeclsParse [])] 
    in unitS (ExpApplication pos [fun, lambda, lamexp, sr, oldredex])
dExp cr (ExpLet pos decls e)        = unitS (ExpLet pos) =>>> dDecls decls =>>> dExp False e
dExp cr (ExpCase pos e alts)        =  -- Use this if no case-tracing!!! unitS (ExpCase pos) =>>> dExp e =>>> dAlts pos alts
    dExp cr e >>>= \e' ->
    newVars pos 2 >>>= \[redex, ne] ->
    lookupVar pos (t_fun 1) >>>= \fun ->
    lookupCon pos tCase >>>= \casenm ->
    getD >>>= \oldredex ->
    setD redex >=>
    dAlts pos alts >>>= \alts' ->
    lookupVar pos (t_ap 1) >>>= \apply ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [apply, sr, oldredex, 
                               ExpApplication pos 
			           [fun, 
				    casenm,
                                    ExpLambda pos [redex, ne] (ExpCase pos ne alts'),
				    sr, oldredex],
			       e'])
dExp cr (ExpIf pos c e1 e2) = 
    dExp cr c >>>= \c' ->
    getD >>>= \oldredex ->
    newVar pos >>>= \t ->
    setD t >=>
    dExp False e1 >>>= \e1' ->
    dExp False e2 >>>= \e2' ->
    makeSourceRef pos >>>= \sr ->
    lookupVar pos t_if >>>= \tif ->
    unitS (ExpApplication pos [tif, sr, c', ExpLambda pos [t] e1', 
                               ExpLambda pos [t] e2', oldredex])
{-
    dExp cr c >>>= \c' ->
    newVars pos 2 >>>= \[redex, ne] ->
    lookupVar pos (t_fun 1) >>>= \fun ->
    lookupCon pos tIf >>>= \ifnm ->
    getD >>>= \oldredex ->
    setD redex >=>
    dExp False e1 >>>= \e1' ->
    dExp False e2 >>>= \e2' ->
    lookupVar pos (t_ap 1) >>>= \apply ->
    lookupVar pos t_value >>>= \value ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [apply, sr, oldredex, 
                               ExpApplication pos 
			           [fun, 
				    ifnm,
                                    ExpLambda pos [redex, ne] (ExpIf pos (ExpApplication pos [value, ne]) e1' e2'),
				    sr, oldredex],
			       c'])
-}
{-
    unitS (ExpIf pos) =>>> doBool c =>>> dExp False e1 =>>> dExp False e2
    where doBool c = 
        lookupVar pos t_value >>>= \value ->
	dExp False c >>>= \c' ->
        unitS (ExpApplication pos [value, c'])
-}
dExp cr (ExpType pos e ctx t)       = unitS (\e' -> ExpType pos e' ctx t) =>>> dExp cr e
dExp cr (ExpApplication pos (f:es))     = 
    case f of
        ExpCon _ _ ->            
	    saturateConstr f es
	_ ->
	    lookupVar pos ((if cr then t_ap else t_rap) (length es)) >>>= \apply ->
	    makeSourceRef pos >>>= \sr -> 
	    dExps True (f:es) >>>= \fes ->
	    getD >>>= \trail ->
            unitS (ExpApplication pos (apply:sr:trail:fes))
dExp cr e@(ExpCon pos id) = saturateConstr e []
dExp cr e@(ExpVar pos id) = 
    lookupName id >>>= \name ->
    getIdArity id >>>= \arity ->
    case arity of
        Nothing -> -- Must be a lambdabound variable
	    if cr then 
	        unitS e
	    else
	        getD >>>= \redex ->
	        lookupVar pos t_indir >>>= \indir ->
	        unitS (ExpApplication pos [indir, redex, e])
--	Just 0  -> unitS e       -- A CAF or a letbound variable with arity 0
	Just n  ->		     -- A letbound or global function
	    makeSourceRef pos >>>= \sr ->
	    getD >>>= \redex ->
	    unitS (ExpApplication pos [e, sr, redex]) 
dExp cr e@(ExpLit pos (LitString _ s)) = -- mkLitString pos e
    -- This is somewhat expensive. But it works...
    lookupCon pos t_Colon >>>= \consid ->
    lookupCon pos t_List >>>= \nilid ->
    let rs = (map (ExpLit pos . LitChar Boxed) s) in
    dExp True (foldr (\c cs -> ExpApplication pos [consid, c, cs]) nilid rs)
dExp cr e@(ExpLit pos (LitInteger b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConInteger >>>= \fci ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fci, sr, d, e])
dExp cr e@(ExpLit pos (LitRational b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConRational >>>= \fcr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fcr, sr, d, e])
dExp cr e@(ExpLit pos lit) = 	-- at a guess, this clause is obsolete.
    dLit lit >>>= \constr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [constr, sr, d, e])
    where dLit (LitInt _ _) = lookupVar pos t_conInt
          dLit (LitChar _ _) = lookupVar pos t_conChar
          dLit (LitInteger _ _) = lookupVar pos t_conInteger
	  dLit (LitRational _ _) = lookupVar pos t_conRational
	  dLit (LitDouble _ _) = lookupVar pos t_conDouble
	  dLit (LitFloat _ _) = lookupVar pos t_conFloat

dExp cr (ExpList pos []) = lookupCon pos t_List >>>= \nil-> wrapConst nil []
dExp cr (ExpList pos es) = 
    lookupCon pos t_Colon >>>= \consid ->
    dExps True es >>>= \es' ->
    foldS 
        (\e es -> wrapConst consid [e, es])
        (lookupCon pos t_List >>>= \nil -> wrapConst nil []) es'
dExp cr e = error ("dExp: no match")


saturateConstr :: Exp Id      -- data constructor
               -> [Exp Id]    -- arguments of the data constructor
               -> DbgTransMonad (Exp Id) -- transformed constructor application

saturateConstr c@(ExpCon pos id) args =
    --trace ("<<< " ++ show id' ++ " -> " ++ show id) $
    dExps True args >>>= \args' ->
    getConArity id >>>= \arity ->
    --trace ("Arity for " ++ show id ++ " is " ++ show arity) $
    if arity > length args' then -- Unsaturated constructor
        lookupCon pos tNTConstr >>>= \ntconstr ->
--        lookupVar pos (t_c (arity - length args')) >>>= \cn ->
        lookupVar pos (t_cn (arity - length args')) >>>= \cn ->
        lookupVar pos (t_pa (length args')) >>>= \pan ->
	getD >>>= \redex ->
	makeSourceRef pos >>>= \sr ->
	unitS (ExpApplication pos [cn, ExpApplication pos (c:args'),
		ExpApplication pos (pan:sr:redex:ExpApplication pos [ntconstr, ExpLit pos (LitInt Boxed id)]:args')])
{-
	unitS (ExpApplication pos [cn, ExpApplication pos 
	                                   [ntconstr, ExpLit pos (LitInt Boxed id)], 
	                           ExpApplication pos (c:args'), sr, redex])
-}
    else
	wrapConst c args'


dGuard :: Exp Id -> DbgTransMonad (Exp Id)

dGuard e@(ExpCon p conid) = unitS e -- Must be the constant True
dGuard e = 
    lookupVar noPos t_value >>>= \value ->
    dExp False e >>>= \e' ->
    unitS (ExpApplication noPos [value, e'])


wrapConst :: Exp Id -> [Exp Id] -> DbgTransMonad (Exp Id)

wrapConst c@(ExpCon pos cid) args =
    getD >>>= \d ->
    lookupVar pos (t_con (length args)) >>>= \con ->
    lookupCon pos tNTConstr >>>= \ntconstr ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos (con:sr:d:c:ExpApplication pos [ntconstr, ExpLit pos (LitInt Boxed cid)]:args))

{-
mkRList wrapper transformer pos ls =    -- Don't do this when debugged list is built-in
    lookupCon pos tCons >>>= \c ->
    let wrapIt [] = wrapper pos =>>> lookupCon pos tNil
        wrapIt (x:xs) = wrapper pos =>>> (unitS cons =>>> transformer x =>>> wrapIt xs)  
        cons e l = ExpApplication pos [c, e, l] 
    in wrapIt ls
-}

mkLitString pos s =
    getD >>>= \d ->
    lookupVar pos t_stringConst >>>= \stringConst  ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [stringConst, sr, d, s])

{-
    lookupVar pos t_const >>>= \c ->
    makeSourceRef pos >>>= \sr ->
    mkRList (\_ -> unitS (\e -> ExpApplication pos [c, d, sr, e]))
            (\e -> unitS (ExpApplication pos [c, d, sr, e]))
            pos
            (map (ExpLit pos . LitChar Boxed) s)
-}

dAlts pos alts =
    mapS dAlt alts >>>= \alts' ->
    newVar pos >>>= \pat ->
    mkFailExpr pos {-Wrong-} >>>= \fpexp ->
    noGuard >>>= \ng ->
    unitS (alts' ++ [Alt pat [(ng, fpexp)] (DeclsParse [])])
    
dAlt (Alt pat gdexps decls) =
    unitS Alt =>>> dPat pat =>>> mapS dGdEs gdexps =>>> dDecls decls


dPats :: [Pat Id] -> DbgTransMonad [Pat Id] 

dPats ps = mapS dPat ps


dPat :: Pat Id -> State Inherited Threaded (Pat Id) Threaded
--dPat :: Pat Id -> DbgTransMonad (Pat Id)
-- Hugs doesn't like it. 

dPat (ExpApplication pos (c:ps)) = 
  wrapR pos =>>> 
  (unitS (ExpApplication pos) =>>> (unitS (c:) =>>> dPats ps))
dPat p@(ExpCon pos id)              = wrapR pos =>>> unitS p
dPat p@(ExpVar pos id)              = unitS p 
dPat p@(ExpLit pos (LitInteger b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConInteger >>>= \pfci ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfci, sr, d, p])
dPat p@(ExpLit pos (LitRational b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConRational >>>= \pfcr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfcr, sr, d, p])
dPat p@(ExpLit pos (LitString _ s)) = 
    foldPatList pos (map (ExpLit pos . LitChar Boxed) s)
dPat p@(ExpLit pos lit)             = wrapR pos =>>> unitS p
dPat p@(ExpList pos [])             = wrapR pos =>>> unitS p
dPat (ExpList pos ps)               = foldPatList pos ps
--wrapR pos =>>> (unitS (ExpList pos) =>>> dPats ps)
dPat (PatAs pos id p)               = unitS (PatAs pos id) =>>> dPat p
dPat p@(PatWildcard pos)            = unitS p 
dPat (PatIrrefutable pos p)         = unitS (PatIrrefutable pos) =>>> dPat p
dPat e                              = error ("dPat: no match ")

foldS f z []     = z 
foldS f z (x:xs) = foldS f z xs >>>= f x 
---f x =>>> foldS f z xs


foldPatList :: Pos -> [Exp Id] -> DbgTransMonad (Exp Id)

foldPatList pos [] =  wrapR pos =>>> lookupCon pos t_List
foldPatList pos (p:ps) = 
    lookupCon pos t_Colon >>>= \consid ->
    wrapR pos =>>> 
    (unitS (\c cs -> ExpApplication pos [consid, c, cs]) 
           =>>> dPat p =>>> foldPatList pos ps) 


wrapR :: Pos -> DbgTransMonad (Exp Id -> Exp Id)

wrapR pos =
    lookupCon pos tR >>>= \r ->
    newVar pos >>>= \wc ->
    unitS (\p -> ExpApplication pos [r, p, wc])
    
{-
handleMain [Fun [{-pat-}] [(gd, e)] decls] =
--    unitS (:[]) =>>> (unitS Fun =>>> dPats pats =>>> dGdEs gdes =>>> dDecls decls)
    dDecls decls >>>= \decls' ->
    case e of
        ExpApplication p [ExpVar pp fid, arg] ->
--        ExpApplication p [ExpApplication _ [ExpVar pp fid, arg], dlg] ->
	    lookupNameStr fid >>>= \fidstr ->
	    case fidstr of
	        "DPrelude.print" ->
		    lookupVar noPos t_dbgprint >>>= \dbgprint ->
		    ok p dbgprint arg {-dlg-} decls'
		"DPrelude.interact" ->
		    lookupVar noPos t_dbginteract >>>= \dbginteract ->
		    ok p dbginteract arg {-dlg -}decls'
		v -> badMain (show v)
	_ -> badMain
    where ok p ide arg {-dlg-} dl =
--	      newVar noPos >>>= \redexvar ->
--	      setD redexvar >=>
	      dExp False arg >>>= \arg' ->
	      unitS [Fun [] [(gd, ExpApplication p [ide, arg'])] dl]
{-
	      unitS [Fun [{-pat-}] [(gd, ExpApplication p 
		                     [ExpApplication p [ide, 
						        ExpLambda p [redexvar] arg'],
				      dlg])] dl]
-}
          badMain = error ("Bad main function . Can only handle 'print' and " ++
                           "'interact' when debugging.\n")
-}

-- Rename the variables in a pattern. Return the new pattern
-- and an association list from the old names to the new ones.
-- Used in transformation of pattern bindings.
patVars (ExpApplication pos ps) = unzipConc (ExpApplication pos) ps
patVars p@(ExpCon pos id)       = unitS (p, [])
patVars (ExpVar pos id)         = newVar pos >>>= \nide -> 
                                  unitS (nide, [((pos, id), nide)])
patVars p@(ExpLit pos lit)      = unitS (p, [])
patVars (ExpList pos ps)        = unzipConc (ExpList pos) ps
patVars (PatAs pos id p)        = newVar pos >>>= \nide@(ExpVar _ nid) ->
                                  patVars p >>>= \(p', vs) ->
				  unitS (PatAs pos nid p', ((pos, id), nide):vs)
patVars p@(PatWildcard pos)     = unitS (p, []) 
patVars (PatIrrefutable pos p)  = unzipConc (PatIrrefutable pos . head) [p]
patVars e                       = error ("patVars: no match")

unzipConc f ps = 
    mapS patVars ps >>>= \psvss ->
    let (ps', vss) = unzip psvss
    in unitS (f ps', concat vss)
    

{- Utility functions --------------------------------------------------------}

makeTuple pos [e] = unitS e
makeTuple pos es = 
    lookupId Con (t_Tuple (length es)) >>>= \tup ->
    unitS (ExpApplication pos (ExpCon pos tup : es))

noGuard =
    unitS (ExpCon noPos) =>>> lookupId Con tTrue

lookupId kind ident = 
  \(Inherited _ _ lookupPrel) s -> (lookupPrel (ident, kind), s)
lookupVar pos ident =  
  \(Inherited _ _ lookupPrel) s -> (ExpVar pos (lookupPrel (ident, Var)), s)
lookupCon pos ident =  
  \(Inherited _ _ lookupPrel) s -> (ExpCon pos (lookupPrel (ident, Con)), s)
lookupName ident = 
  \_ s@(Threaded istate _ _) -> (lookupIS istate ident, s)
lookupNameStr ident = 
  \_ s@(Threaded istate _ _) -> (strIS istate ident, s)


{- Create a new variable with given position -}
newVar :: Pos -> DbgTransMonad (Exp Id)

newVar pos = \_ (Threaded istate srt idt) ->
                 case uniqueIS istate of
	             (i, istate') -> (ExpVar pos i, Threaded istate' srt idt)


{- Create a list of n new variables, all with the same given position -}
newVars :: Pos -> Int -> DbgTransMonad [Exp Id]

newVars pos n = \_ (Threaded istate srt idt) ->
                    case uniqueISs istate [1..n] of
	                (is, istate') -> (map (ExpVar pos . snd) is, 
			                  Threaded istate' srt idt)

{-
Make info for a variable with given Id, name, arity and type.
Right Id: Id is added to name
Left Id: name used as given 
-}
mkInfo :: Either Id Id -> String -> Int -> NewType -> Info

mkInfo (Right u) str arity nt = 
    InfoVar u (visImpRev (str ++ "_" ++ show u)) (InfixDef, 9) 
      IEnone nt (Just arity)

mkInfo (Left u) str arity nt = 
    InfoVar u (visImpRev str) (InfixDef, 9) IEnone nt (Just arity)


{-
Wraps off most of the SRs, Traces and Rs.
e.g. 
unwrapNT 0 True (R Bool) = R Bool
unwrapNT 1 False (SR -> Trace -> R(Trace -> R Int -> R Bool)) = 
  Trace -> R Int -> R Bool
unwrapNT 2 False 
  (SR -> Trace -> R(Trace -> R Int -> R(Trace -> R Char -> R Int))) =
  Trace -> R Int -> R Char -> R Int
-}
unwrapNT :: Int -> Bool -> NewType -> NewType

unwrapNT arity isCaf nt@NoType = nt
unwrapNT arity isCaf 
  (NewType free exist ctxs [NTcons arrow [sr, NTcons _ [t, rt]]]) = 
    NewType free exist ctxs (if isCaf 
                               then [dStripR arity rt] 
                               else [NTcons arrow [t, dStripR arity rt]])
  where 
  dStripR 0 t = t
  dStripR n (NTcons rt [NTcons a1 [t, NTcons a2 [a, b]]]) 
    | a1 == arrow && a1 == a2 = NTcons arrow [a, dStripR (n-1) b]
  dStripR n t@(NTcons rt [a]) = t
unwrapNT arity isCaf nt = error ("unwrapNT: strange type: " ++ show nt)


{-
Create a new identifier with given arity, name and type.
Boolean argument decides if Id is appended to name.
-}
addNewName :: Int -> Bool -> String -> NewType -> DbgTransMonad Id

addNewName arity addIdNr str nt = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let info = mkInfo (if addIdNr then Right i else Left i) str arity nt
	    istate'' = addIS i info istate'
	in (i, Threaded istate'' srt idt)

{-
Create a new primitive identifier with given Info, changing just the
location in the table (i.e. the lookup key).
-}
addNewPrim :: Info -> DbgTransMonad Id

addNewPrim (InfoVar _ (Qualified m nm) fix ie nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let newNm = Qualified m (packString (unpackPS nm++"'"))
            info' = InfoVar i newNm fix IEnone NoType ar
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)
addNewPrim (InfoVar _ nm fix ie nt ar) = 
  error ("In tracing transformation: foreign import has unqualified name?")

{-
Create a new identifier for the prim-wrapper, given prim Info, in a fresh
location in the table
-}
addNewWrapper :: Info -> DbgTransMonad Id

addNewWrapper (InfoVar _ nm fix ie nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let info' = InfoVar i nm fix ie NoType (Just 2)
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)

{-
Overwrite the original primitive identifier with new Info, reflecting
the change in type and arity.
-}
overwritePrim :: Id -> Inherited -> Threaded -> Threaded

overwritePrim i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i nm fix ie _ _) = InfoVar i nm fix ie NoType (Just 2)
      in Threaded (updateIS istate i updI) srt idt

{-
Overwrite the original primitive identifier with new name.
-}
overwriteOrigName :: Id -> Inherited -> Threaded -> Threaded
overwriteOrigName i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i (Qualified m f) fix ie nt ar) =
                InfoVar i (Qualified m (packString ('\'': unpackPS f)))
                                          fix ie nt ar
      in Threaded (updateIS istate i updI) srt idt



setD :: Exp Id -> DbgTransMonad Inherited

setD d = \(Inherited _ f lookupPrel) s -> 
           (Inherited d f lookupPrel, s) 


getD :: DbgTransMonad (Exp Id)

getD = \(Inherited d _ _) s -> (d, s)


normalFail = 
    lookupVar noPos t_fatal >>>= \fail ->
    setFail fail

setFail fail = \(Inherited d _ lookupPrel) s -> 
                 (Inherited d fail lookupPrel, s) 

getFail = \(Inherited _ fail _) s -> (fail, s)


{- old, were not used anywhere
getModStr = \(Inherited _ _ _ modstr) s@(Threaded istate _ _) -> (modstr, s)
setModStr modstr = \(Inherited d f lp _) s -> (Inherited d f lp modstr, s)
-}


getConArity id = \_ s@(Threaded istate _ _) -> (arityIS istate id, s)

getIdArity id = 
    \_ s@(Threaded istate _ _) ->
    case lookupIS istate id of
        Nothing -> (Nothing, s)
	Just info -> (Just (arityVI info){-(arityIS istate id)-}, s)

{-
setArity arity id = \_ (Threaded istate srs) ->
                        Threaded (updVarArity noPos id arity istate) srs
-}


getArity :: [Fun a] -> Int

getArity (Fun pats _ _ : _) = length pats


setArity :: Int -> Int -> a -> Threaded -> Threaded

setArity arity id  = \inh (Threaded (IntState unique rps st errors) srt idt) ->
  let newid = case lookupAT st id of
                Just (InfoMethod u tid fix nt _ cls) ->
                      InfoMethod u tid fix nt (Just arity) cls
                Just (InfoDMethod u tid nt _ cls) ->
                      InfoDMethod u tid nt (Just arity) cls
       	        Just (InfoIMethod u tid nt _ cls) ->
	              InfoIMethod u tid nt (Just arity) cls
	        Just (InfoVar u tid fix exp nt _) ->
	             InfoVar u tid fix exp nt (Just arity)
  in Threaded (IntState unique rps (updateAT st id (\_ -> newid)) errors) 
       srt idt


getPositions :: DbgTransMonad (Int,[Int])
getPositions = \_ s@(Threaded _ srt _) -> (srt, s)


setPositions :: (Int,[Int]) -> a -> Threaded -> Threaded
setPositions srt = \_ s@(Threaded istate _ idt) -> Threaded istate srt idt


makeSourceRef :: Pos -> DbgTransMonad (Exp Id)

makeSourceRef p (Inherited _ _ lookupPrel) s@(Threaded is (nsr, srs) idt) =
  if rowcol == head srs then
    (ExpApplication p [ExpCon p sr3, ExpLit p (LitInt Boxed nsr)], s)
  else
    let nsr' = nsr+1
    in seq nsr' (ExpApplication p [ExpCon p sr3, ExpLit p (LitInt Boxed nsr')],
                 Threaded is (nsr', rowcol:srs) idt)
  where 
  (row, col) = fromPos p 
  rowcol = 10000*row + col
  sr3 = lookupPrel (tSR3, Con)


{-
Add identifier with position to threaded list.
-}
addId :: (Pos,Id) -> a -> Threaded -> Threaded

addId pid inh (Threaded is srt idt) = Threaded is srt (pid:idt)


{- Create expression for a string literal at given position -}
eStr :: Pos -> [Char] -> Exp a

eStr pos s = ExpLit pos (LitString Boxed s)


stripPrelude ('_':'x':s) = s
stripPrelude ('@':s) = s
stripPrelude s = s


{- is it a class method? -}
isCMethod :: Info -> Bool

isCMethod (InfoIMethod _ _ _ _ _) = True
isCMethod (InfoDMethod _ _ _ _ _) = True
isCMethod _ = False

{- End Module DbgTrans ------------------------------------------------------}
