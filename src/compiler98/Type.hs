module Type(typeTopDecls,bindType) where

import List(partition)
import TokenId
import Syntax
import SyntaxPos
import Kind
import State
import NT
import TypeLib
import TypeEnv
import Extra
import MergeSort(group,unique)
import TypeSubst
import TypeCtx
import TypeData
import PackedString(PackedString,packString,unpackPS)
import Info
import IntState
import TypeUtil
import TypeUnify
import Nice
import Bind(identPat)
import Extract(type2NT)
import AssocTree
import Tree234
import Remove1_3

--import PPSyntax		-- just for debugging
--import StrSyntax	-- just for debugging

typeTopDecls tidFun defaults state code dbgtrans topdecls =
  let defaults' =
        case defaults of
	  Nothing -> [tidFun (tInteger,TCon),tidFun (tDouble,TCon)]
	  Just def -> def 
      result =  typeTopDeclScc topdecls tidFun defaults' dbgtrans finalState state
      finalState = snd result
  in case result of
       (topdecls,state) ->
          case mapS typeCode code  tidFun  state of
            (code,state) ->
	       (code,topdecls,state)

typeCode d@(CodeClass pos cls) tidFun state = (d,state)
typeCode (CodeInstance pos cls typ _ _ ms) tidFun state =
  let clsInfo = dropJust (lookupIS state cls)
      sc = superclassesI clsInfo
      (free,ctxs) = dropJust (lookupAT (instancesI clsInfo) typ)
  in case uniqueISs state ctxs of
      (ctxsi,state) ->
	(CodeInstance pos cls typ (map snd ctxsi)
	     (map (buildCtx state noPos (map (mapFst (mapSnd NTvar)) ctxsi) . ( \ cls -> TypeDict cls (NTcons typ (map NTvar free)) [(0,pos)])) sc) ms
	,state)


typeTopDeclScc (DeclsScc xs) tidFun defaults dbgtrans finalState state =
     case mapS typeDepend xs (TypeDown initEnv tidFun defaults [] [] dbgtrans) (TypeState state idSubst initCtxs []) of
	(xs,TypeState state phi ctxs ectxsi) -> (DeclsScc (concat xs),state)


typeDeclScc (DeclsScc xs) =
  mapS typeDepend xs >>>= \ xs ->
  unitS (DeclsScc (concat xs))


typeDepend (DeclsNoRec d13) =
  fixDecl13 d13 >>>= \ d -> 
  typeScc [d] >>>= \ (dicts,[d]) ->
  unitS (map DeclsNoRec dicts ++ [DeclsNoRec d])

typeDepend (DeclsRec ds13) =
  mapS fixDecl13 ds13 >>>= \ ds ->
  typeScc ds >>>= \ (dicts,ds) ->
  unitS (map DeclsNoRec dicts ++ [DeclsRec ds])

--- ======== The hairy part

typeScc decls down@(TypeDown env tidFun defaults ctxsDict envDict dbgtrans) up@(TypeState state phi inCtxs ectxsi) = 
  let -- ctxs should only get up but the monad can not handle that!
      trueExp :: Exp Int
      trueExp = ExpCon noPos (tidFun (tTrue,Con))

      nextTvar :: Int
      (nextTvar,_) = uniqueIS state -- take a look at the next type variable

      envHere :: [(Int, NT)]
      (envHere,up'@(TypeState state' phi' ctxs' ectxsi')) =
		envDecls decls () (TypeState state phi initCtxs [])

      decls' :: [Decl Int]
      (decls',up''@(TypeState state'' phi'' ctxs'' existCtxsi)) =
		mapS typeDecl decls (TypeDown (envHere++env) tidFun defaults (usedCtx ++ ctxsDict) (derivedDict ++ envDict) dbgtrans) up'

      phiEnv :: [(Int,NT)]
      phiEnv     = substEnv phi'' env
      phiEnvHere :: [(Int,NT)]
      phiEnvHere = substEnv phi'' envHere
      phiCtxs :: [TypeDict]
      phiCtxs    = substCtxs phi'' ctxs''

      globalTVars :: [Int]
      globalTVars = (unique . tvarsInEnv) phiEnv

      uniqueCtxs :: [TypeDict]
      uniqueCtxs =	-- These contexts are needed
		( map ( \ (TypeDict c v ip:cvips) -> TypeDict c v (ip++concatMap ( \ (TypeDict c v ip) -> ip) cvips))
		. group
		)  phiCtxs

      existCtxs :: [(Int,NT)]
      existCtxs = map fst existCtxsi

      globalCtxs :: [(Int,NT)]
      localCtxs0 :: [(Int,NT)]
      (globalCtxs,localCtxs0) =   -- These are the simplified context that are needed
		( partition  ( \ (c,nt) -> stripNT nt `elem` globalTVars)
	        . ctxsReduce state
 		. concatMap (ctxsSimplify state [])
--                . map ( \ (TypeDict c nt _) -> (c,nt))
                ) uniqueCtxs

      localCtxs :: [(Int,NT)]
      localCtxs = filter (`notElem` existCtxs) localCtxs0


    	-- get some unique identifiers

      globalCtxsi :: [((Int,NT),Int)]
      (globalCtxsi,state1) = uniqueISs state'' globalCtxs

      localCtxsi :: [((Int,NT),Int)]
      (localCtxsi,state2) = uniqueISs state1 localCtxs

	-- Contexts that depend on type variables that are not all quantified

      outCtxs :: [TypeDict]
      outCtxs = map ( \ ((c,nt),i) -> TypeDict c nt [(i,noPos)]) globalCtxsi

	-- Set up argument and available dictionaries for identifiers without a given type

      derivedArgs :: [(Int,Exp Int)]
      derivedArgs = map ( \ ((c,nt),i) -> (stripNT nt,ExpVar noPos i)) localCtxsi
      derivedDict :: [(Int,[Exp Int])]
      derivedDict = map ( \ (i,nt) -> (i,(map snd . filter ((`elem` freeNT nt) . fst)) derivedArgs))  phiEnvHere   -- default is fixed in bindType

	-- Create mapping from Int to expression for requested dictionary

      usedCtx :: [(Int,Exp Int)]
      usedCtx = concatMap ( \ (TypeDict c nt ips) -> let dict = buildCtx state noPos (existCtxsi++localCtxsi++globalCtxsi) (TypeDict c nt ips)
						     in map ( \ (i,p) -> (i,dict)) ips)
		          uniqueCtxs

	-- Insert types into state (comparing with given type if such exist) and add dictionary arguments

      declsDict :: [Decl Int]
      declsDict = map ( \ (_,i) ->
			 case lookup i ctxsDict of
			   Just exp -> DeclFun noPos i [Fun [] [(trueExp,exp)] (DeclsScc [])]) globalCtxsi 

      (decls'',state3) = mapS bindType decls' (globalTVars,trueExp,localCtxsi,phiEnvHere,defaults) state2
  in
--	strace ("usedCtx = " ++ show (map fst usedCtx)) $
--	strace ("globalCtxs = " ++ show globalCtxs) $
--	strace ("localCtxs0 = " ++ show localCtxs0) $
--	strace ("existCtxs = " ++ show existCtxs) $
--	strace ("phi'' = " ++ show phi'') $
--	strace ("ctxs'' = " ++ show ctxs'') $
--	strace ("phiCtxs = " ++ show phiCtxs) $

     ((declsDict,decls''),TypeState state3 (stripSubst phi'' nextTvar) (outCtxs++inCtxs) existCtxsi)


--------------------- Fix arguments and types

isExist (NTexist _) = True
isExist _           = False


bindType :: Decl Int -> ([Int], Exp Int, [((Int, NT), Int)], [(Int, NT)], [Int]) -> IntState -> (Decl Int, IntState)
bindType decl@(DeclPat (Alt pat gdexps decls)) down@(globalTVars,trueExp,[],envHere,defaults) state = -- No context for left hand patterns!
  case mapS0 checkType (identPat pat) down state of
    state -> (decl,state)
 where
  checkType (pos,ident) (globalTVars,trueExp,localCtxi,envHere,defaults) state =
    case lookup ident envHere of
      Nothing -> strace ("Nothing derived for " ++ show ident ++ " at " ++ strPos pos) state	-- Nothing derived for this one
      Just derivedNT ->
	let derivedFree = filter (`notElem` globalTVars) (snub (freeNT derivedNT))
        in case ntIS state ident of
            (NoType,state) ->
	      updateIS state ident (newNT (NewType derivedFree [] [] [polyNT derivedFree derivedNT]))
            (given@(NewType givenFree [] givenCtx [givenNT]),state) -> 
	      if length givenFree /= length derivedFree
	      then addError state ("Derived type " ++ show derivedFree ++ niceNT Nothing state (mkALNT derivedNT) derivedNT ++ " is not an instance of " 
		   		       ++ show givenFree
				       ++  niceNT Nothing state (mkAL givenFree) givenNT ++ " at " ++ strPos pos)
	      else state

bindType decl@(DeclPat (Alt pat gdexps decls)) down@(globalTVars,trueExp,localCtxsi,envHere,defaults) state = -- No context for left hand patterns!
  (decl,addError (foldr typeError state (identPat pat)) errmsg)
 where
  errmsg = "Context for " ++ mixCommaAnd ((map (strIS state) . unique . map (fst . fst)) localCtxsi)
					++ " needed in left hand pattern at " ++ strPos (getPos pat) ++ "."
  nt = NewType [1] [] [] [NTvar 1]   -- Used instead of derived type, not sure if it is any idea
  typeError (pos,ident) state = (updateIS state ident (newNT nt))

bindType decl@(DeclPrimitive pos fun arity typ) down state =
  (decl,state)
bindType decl@(DeclForeignImp pos str fun arity cast typ) down state =
  (decl,state)
bindType decl@(DeclForeignExp pos str fun typ) down state =
  (decl,state)
bindType decl@(DeclFun pos fun funs) (globalTVars,trueExp,localCtxi,envHere,defaults) state =
  case ntIS state fun of
    (NoType,state) ->
      let derivedNT = assocDef  envHere (error "162") fun
	  derivedFree = filter (`notElem` globalTVars) (snub (freeNT derivedNT))
	  derivedCtxi :: [((Int,NT),Int)]
	  defaultCtxi :: [((Int,NT),Int)]
	  (derivedCtxi,defaultCtxi) = partition ((`elem` derivedFree) . stripNT . snd . fst) localCtxi
      in  case filter (isExist . snd . fst) defaultCtxi of
	    [] -> 
               case buildDefaults pos defaultCtxi trueExp defaults state of
	         (defaultDecls,state) ->
	            (DeclFun pos fun (map (bindFun (map (\ (c_v,i) -> ExpVar pos i) derivedCtxi) (map DeclsNoRec defaultDecls))  funs)
	            ,updateIS state fun (newNT (NewType derivedFree [] (map (mapSnd stripNT . fst) derivedCtxi) [polyNT derivedFree derivedNT]))
	            )
	    er -> (decl
	  	  ,addError state ("Dictionary can not be found for existential types, error detected at " ++ strPos pos)
	  	  )

    (given@(NewType givenFree [] givenCtx [givenNT]),state) -> 
      let derivedNT = assocDef envHere (error "171")  fun
	  derivedFree = filter (`notElem` globalTVars) (snub (freeNT derivedNT))
      in case unify state idSubst (derivedNT,givenNT) of 
	  Left (phi,err) ->
		(decl,addError state ("Derived type for " ++ strIS state fun ++ " at " ++ strPos pos 
                                       ++ " can not be unified with given type due to " ++ err
                                       ++ "\nDerived:" ++ niceNT Nothing state (mkALNT derivedNT) derivedNT
                                       ++ "\nGiven  :" ++ niceNT Nothing state (mkAL givenFree) givenNT))
	  Right phi ->
		let phis = ( foldr (\ (k,v) t -> addAT t (++) k [v]) initAT
                           . map fixSubst 
                           . treeMapList (:)
                           ) phi

                    al = mkAL (givenFree ++ freeNT derivedNT)

		    fixSubst (v,NTvar v') = if v `elem` givenFree then (v',NTvar v) else (v,NTvar v')
		    fixSubst (v,NTany v') = if v `elem` givenFree then (v',NTvar v) else (v,NTvar v')
                    fixSubst p = p

                    one2many phi = ( filter ((/= 1).length.snd)
                                   . treeMapList (:)) phis
                    freebound phi = ( filter ((`elem` givenFree).fst)
                                    . filter ((== 1).length . snd)
                                    . treeMapList (:)
                                    ) phis

                    safePhi phi v =
                      case lookupAT phi v of
                         Nothing -> Just v
                         Just xs -> findV xs

                    findV [NTvar v] = Just v
                    findV [NTany v] = Just v
                    findV _ = Nothing

		    sameNT (NTany _) v   = NTany v
		    sameNT (NTvar _) v   = NTvar v
		    sameNT (NTexist _) v = NTexist v


                    localCtxi' = ( map dropJust
                                 . filter isJust
                                 . map ( \ ((c,nt),i) -> case safePhi phis (stripNT nt) of
						          Just v -> Just ((c,sameNT nt v),i)
							  Nothing -> Nothing) -- either one2many of freebound
                                 ) localCtxi
		    (derivedCtxi,defaultCtxi) = partition ((`elem` givenFree) . stripNT . snd . fst) localCtxi'
		    derivedIntPairI = map (mapFst (mapSnd stripNT)) derivedCtxi

                    ctxcheck ctxi = filter ((`notElem` givenCtx) . fst) ctxi

                in case (one2many phis,freebound phis,ctxcheck derivedIntPairI) of
                  ([],[],[]) ->  -- Type is ok, but we need to introduce new unique ints for excessive contexts, eg, (Eq a,Ord a)
		    let 
                        (mGivenCtxi',state2) = (uniqueISs state . zip givenCtx . map (\ cv -> lookup cv derivedIntPairI)) givenCtx
                        givenCtxi' = map stripAndFix mGivenCtxi'
                        stripAndFix ((c,Nothing),u) = (c,u)
                        stripAndFix ((c,Just v),u) = (c,v)
                    in  case filter (isExist . snd . fst) defaultCtxi of
	               [] -> 
		          case buildDefaults pos defaultCtxi trueExp defaults state2 of
		             (defaultDecls,state3) ->
		               (DeclFun pos fun (map (bindFun (map (\ (c_v,i) -> ExpVar pos i) givenCtxi') (map DeclsNoRec defaultDecls))  funs)
                               ,state3
		               )
 	               er ->
			(decl
			,addError state ("Dictionary can not be found for existential types, error detected at " ++ strPos pos 
					     ++ " (signature given)"))
                  (eOne2Many,eFreeBound,eCtxs) ->
                    let sOne2Many xs = concatMap ( \ (v,nts) -> "\n    type variable " ++ niceNT Nothing state al (NTvar v) ++ " bound to "
						             ++ mixCommaAnd (map (niceNT Nothing state al) nts)) xs
                        sFreeBound [] = []
                        sFreeBound [(v,t)] = "\n    given free variable " ++ niceNT Nothing state al (NTvar v)
                                              ++ " is bound to " ++ niceNT Nothing state al (head t)
                        sFreeBound xs = "\n    given free variables " ++ mixCommaAnd (map (niceNT Nothing state al . NTvar . fst) xs)
                                     ++ " are bound to " ++ mixCommaAnd (map (niceNT Nothing state al . head . snd) xs)
                                     ++ " respectively"
                        sCtxs [] = ""
                        sCtxs xs = "\n    different contexts"
                    in 
		     (decl
                     ,addError state ("Derived type for " ++ strIS state fun ++ " at " ++ strPos pos 
                                       ++ " does not match due to:" ++ sOne2Many eOne2Many ++ sFreeBound eFreeBound ++ sCtxs eCtxs
                                       ++ "\nDerived:" ++ niceCtxs Nothing state al (map fst derivedIntPairI) ++ niceNT Nothing state al derivedNT
                                       ++ "\nGiven  :" ++ niceCtxs Nothing state al givenCtx ++ niceNT Nothing state al givenNT))

bindFun dictArgs defaultDecls (Fun args gdexps (DeclsScc decls)) =
  Fun (dictArgs++args) gdexps (DeclsScc (defaultDecls ++ decls))


      

--------------------- Nice


typeDecl d@(DeclPrimitive pos fun arity typ) =
  unitS d
typeDecl d@(DeclForeignImp pos str fun arity cast typ) =
  unitS d
typeDecl d@(DeclForeignExp pos str fun typ) =
  unitS d
typeDecl (DeclPat (Alt pat gdexps decls)) =
  typePat pat        >>>= \(pat,patT,eTVar) ->
  typeDeclScc decls    >>>= \decls ->
  typeGdExps gdexps  >>>= \(gdexps,gdexpsT) ->
  typeUnify (msgPat pat) patT gdexpsT >>>= \_ ->
--  checkExist [] eTVar >>> 
  unitS (DeclPat (Alt pat gdexps decls))
typeDecl (DeclFun pos fun funs) =
  typeIdentDef id pos fun        >>>= \ (_,funT) -> 
  typeNewTVar >>>= \retT ->
  getIdent (t_Arrow,TCon) >>>= \arrow ->
  mapS (typeFun fun arrow funT retT) funs        >>>= \funs ->
  unitS (DeclFun pos fun funs)

typeFun fun arrow funT retT (Fun args13 gdexps decls) =
  mapS fixPat13 args13   >>>= \ args ->
  envPats args       >>>= \ argsE ->
  getEnv	     >>>= \ oldEnv ->
  extendEnv argsE    >=>
  mapS typePat args  >>>= \args ->
  case unzip3 args of
    (args,argsT,eTVar) -> 
       typeUnify (msgFun gdexps) funT (funType arrow (argsT++[retT])) >>>= \_ ->
       typeDeclScc decls    >>>= \decls ->
       typeGdExps gdexps  >>>= \(gdexps,gdexpsT) ->
       typeUnify (msgFun gdexps) retT gdexpsT >>>= \_ ->
       checkExist oldEnv (concat eTVar) >>> 
       unitS (Fun args gdexps decls) -- ,funType arrow (argsT++[gdexpsT])))



typeLit e@(ExpLit pos (LitInteger _ _)) =    --- Add fromInteger
  debugTranslating >>>= \dbgtrans ->
  if dbgtrans then
      getIdent (tInteger,TCon) >>>= \tcon -> unitS (e,NTcons tcon [])
  else
      getIdent (tInteger,TCon) >>>= \ tcon ->
      getIdent (tfromInteger,Var) >>>= \ tfromInteger ->
      typeIdentDict (ExpVar pos) pos tfromInteger >>>= \ (exp,expT) ->
      typeUnifyApply (msgLit pos "integer") [expT,NTcons tcon []] >>>= \ t ->
      unitS (ExpApplication pos [exp,e],t)
typeLit e@(ExpLit pos (LitRational _ _)) =	--- Add fromRational
  getIdent (tRational,TCon) >>>= \ tcon ->
  getIdent (tfromRational,Var) >>>= \ tfromRational ->
  typeIdentDict (ExpVar pos) pos tfromRational >>>= \ (exp,expT) ->
  typeUnifyApply (msgLit pos "rational") [expT,NTcons tcon []] >>>= \ t ->
  unitS (ExpApplication pos [exp,e],t)
typeLit e@(ExpLit pos (LitString _ _)) = getIdent (tString,TCon) >>>= \tcon -> unitS (e,NTcons tcon [])
typeLit e@(ExpLit pos (LitInt _ _))    = getIdent (tInt,TCon)    >>>= \tcon -> unitS (e,NTcons tcon [])
typeLit e@(ExpLit pos (LitFloat _ _))  = getIdent (tFloat,TCon)  >>>= \tcon -> unitS (e,NTcons tcon [])
typeLit e@(ExpLit pos (LitDouble _ _)) = getIdent (tDouble,TCon) >>>= \tcon -> unitS (e,NTcons tcon [])
typeLit e@(ExpLit pos (LitChar _ _))   = getIdent (tChar,TCon)   >>>= \tcon -> unitS (e,NTcons tcon [])

typeUnifyBool g t =
  getIdent (tBool,TCon) >>>= \ tcon ->
  typeUnify (msgBool g) (NTcons tcon []) t

typeGdExp (g,e) =
  typeExp g >>>= \(g,gT) ->
  typeUnifyBool g gT >>>= \_ ->
  typeExp e >>>= \(e,eT) ->
  unitS ((g,e),eT)

typeGdExps gdexps =
  mapS typeGdExp gdexps >>>= \ gdexps ->
  case unzip gdexps of
   (gdexps,gdexpsT) -> 
     typeUnifyMany (msgGdExps gdexps) gdexpsT >>>= \ t ->
     unitS (gdexps,t)

typeAlt (Alt pat13 gdexps decls) =
  fixPat13 pat13	    >>>= \pat ->
  envPat pat                >>>= \patE -> 
  getEnv	     >>>= \ oldEnv ->
  extendEnv patE            >=>
  typePat pat               >>>= \ (pat,patT,eTVar) ->
  typeDeclScc decls           >>>= \decls ->
  typeGdExps gdexps         >>>= \(gdexps,gdexpsT) ->
  checkExist oldEnv eTVar >>> 
  unitS (Alt pat gdexps decls,(patT,gdexpsT))


typeExp (ExpRecord exp fields) = removeExpRecord exp fields >>>= typeExp
typeExp (ExpDo pos stmts) = removeDo stmts >>>= typeExp
typeExp (ExpCase pos exp alts) =
  typeExp exp >>>= \(exp,expT) ->
  mapS typeAlt alts >>>= \alts ->
  case unzip alts of
    (alts,altsT) ->
      case unzip altsT of
	(altsTP,altsTE) ->
          typeUnifyMany (msgAltPats alts) altsTP >>>= \ patT ->
          typeUnify     (msgCase exp)  expT patT >>>= \ _ ->
          typeUnifyMany (msgAltExps alts) altsTE >>>= \ t ->
          unitS (ExpCase pos exp alts,t)

typeExp (ExpIf pos cond exp1 exp2)   =
  typeExp cond           >>>= \ (cond,condT) ->
  typeUnifyBool cond condT    >>>= \_ ->
  typeExp exp1           >>>= \ (exp1,exp1T) ->
  typeExp exp2           >>>= \ (exp2,exp2T) ->
  typeUnify (msgIf exp1 exp2) exp1T exp2T  >>>= \ t ->
  unitS (ExpIf pos cond exp1 exp2,t)

typeExp (ExpApplication pos es)   =
  mapS typeExp es >>>= \es ->
  case unzip es of
    (es,esT) -> 
      typeUnifyApply (msgApply es) esT >>>= \ t ->
      unitS (ExpApplication pos es,t)

typeExp e@(ExpVar pos ident)        =
  typeIdentDict (ExpVar pos) pos ident

typeExp (ExpCon pos ident)        =
  typeExpCon pos ident

typeExp e@(ExpLit pos lit)        =
  typeLit e

typeExp (ExpList  pos es)         =
  mapS typeExp es            >>>= \es ->
  case unzip es of
   (es,esT) -> 
     typeUnifyMany (msgList es) esT >>>= \t ->
     getIdent (t_List,TCon)     >>>= \tcon ->
     unitS (ExpList pos es,NTcons tcon [t])
 where
  mapL = map :: ((a->b) -> [a] -> [b])

typeExp (ExpLambda pos pats13 exp)  =
--  phiMark >>>= \ mark ->
  mapS fixPat13 pats13 >>>= \ pats ->
  envPats pats   >>>= \ patsE ->
  getEnv	     >>>= \ oldEnv ->
  extendEnv patsE >=>
  mapS typePat pats   >>>= \ pats ->
  typeExp exp    >>>= \ (exp,expT) ->
  getIdent (t_Arrow,TCon) >>>= \arrow ->
  case unzip3 pats of
     (pats,patsT,eTVar) -> -- phiPrune2 "Lambda" patsE (funType arrow (patsT++[expT])) >>>= \ t ->
		     checkExist oldEnv (concat eTVar) >>> 
		     unitS (ExpLambda pos pats exp,funType arrow (patsT++[expT]))

typeExp (ExpLet pos decls exp)    =
  typeDeclScc decls >>>= \decls ->
  typeExp exp     >>>= \(exp,expT) ->
  unitS (ExpLet pos decls exp,expT)

typeExp (ExpType pos exp ctxs t) = -- Ignoring ctx and doesn't check if the free variables really are free !!!
  (if not (null ctxs) 
  then strace ("Context at " ++ strPos (getPos ctxs) ++ " in typed expression is ignored :-(")
  else id) $
  typeExp exp >>>= \ (exp,expT) ->
  let nt = type2NT t
      free = snub (freeNT nt)
  in 
    mapS (\ _ -> typeNewTVar) free >>>= \ free' -> 
    let phi = list2Subst (zip free (map ( \ (NTany a) -> NTvar a) free')) 
    in typeUnify (msgExpType pos) (subst phi nt) expT >>>= \ expT ->
       unitS (exp,expT)

typeExp (PatWildcard pos) =
  typeNewTVar >>>= \tvar ->
  unitS (PatWildcard pos,tvar)

typeExp e =
  getTypeErrors >>>= \errs->
  error ("when typing expression at " ++ strPos (getPos e) ++ "\n"++
         unlines errs)  --  ++ "\n" ++ ppExp False dummyIntState e 0)


-------------------

typePat (ExpApplication pos es)   =
  mapS typePat es >>>= \es ->
  case unzip3 es of
    (es,esT,eTVar) -> 
      typeUnifyApply (msgApply es) esT >>>= \ t ->
      unitS (ExpApplication pos es,t,concat eTVar)

typePat e@(ExpVar pos ident)        =
  typeIdentDict (ExpVar pos) pos ident >>>= \ (e,t) ->
  unitS (e,t,[])

typePat (ExpCon pos ident)        =
  typePatCon pos ident

typePat e@(ExpLit pos lit)        =
  typeLit e >>>= \ (e,t) ->
  unitS (e,t,[])

typePat (ExpList  pos es)         =
  mapS typePat es            >>>= \es ->
  case unzip3 es of
   (es,esT,eTVar) -> 
     typeUnifyMany (msgList es) esT >>>= \t ->
     getIdent (t_List,TCon)     >>>= \tcon ->
     unitS (ExpList pos es,NTcons tcon [t],concat eTVar)
 where
  mapL = map :: ((a->b) -> [a] -> [b])

typePat (ExpType pos exp ctxs t) = -- Ignoring ctx and doesn't check if the free variables really are free !!!
  (if not (null ctxs) 
  then strace ("Context at " ++ strPos (getPos ctxs) ++ " in typed expression is ignored :-(")
  else id) $
  typePat exp >>>= \ (exp,expT,eTVar) ->
  let nt = type2NT t
      free = snub (freeNT nt)
  in 
    mapS (\ _ -> typeNewTVar) free >>>= \ free' -> 
    let phi = list2Subst (zip free (map ( \ (NTany a) -> NTvar a) free')) 
    in typeUnify (msgExpType pos) (subst phi nt) expT >>>= \ expT ->
       unitS (exp,expT,eTVar)

typePat (PatAs             pos ident pat) =
  typeIdentDef (PatAs pos) pos ident >>>= \ (patas,identT) ->
  typePat pat     >>>= \ (pat,patT,eTVar) ->
  typeUnify (msgAs pos) identT patT >>>= \ t ->
  unitS (patas pat,t,eTVar)

typePat e@(PatWildcard       pos) = 
  typeNewTVar >>>= \tvar ->
  unitS (e,tvar,[])
typePat (PatIrrefutable    pos pat) =
  typePat pat >>>= \ (pat,patT,eTVar) ->
  unitS (PatIrrefutable pos pat,patT,eTVar)

typePat (PatNplusK         pos n n' k le sub) =
  typeIdentDict (ExpVar pos) pos n  >>>= \ (ExpVar _ n, typN) ->
  typeIdentDict (ExpVar pos) pos n' >>>= \ (ExpVar _ n', typN') ->
  typeExp k                         >>>= \ (k,typK) ->
  typeExp le                        >>>= \ (le,typLE) ->
  typeUnifyBool le typLE            >>>= \ _ ->
  typeExp sub                       >>>= \ (sub,typSUB) ->
  typeUnify (msgNK pos) typK typN   >>>= \ t ->
  typeUnify (msgNK pos) typN' typN  >>>= \ t ->
  typeUnify (msgNK pos) typSUB typN >>>= \ t ->
  unitS (PatNplusK pos n n' k le sub,t,[])

typePat e                         = error ("typePat " ++ strPos (getPos e))


-------------------

-- fixPat13 change 1.3 patterns into 1.2 patterns
-- No InfixList at this point!

fixDecl13 (DeclPat (Alt pat gdexps decls)) = fixPat13 pat >>>= \ pat -> unitS (DeclPat (Alt pat gdexps decls))
fixDecl13 decl = unitS decl

fixPat13 (ExpRecord	 exp fields)  = removeExpRecord exp fields >>>= fixPat13
fixPat13 (ExpApplication pos pats)    = unitS (ExpApplication pos) =>>> mapS fixPat13 pats
fixPat13 (ExpList        pos pats)    = unitS (ExpList pos)        =>>> mapS fixPat13 pats
fixPat13 (PatAs          pos tid pat) = unitS (PatAs pos tid)      =>>> fixPat13 pat
fixPat13 (PatIrrefutable pos pat)     = unitS (PatIrrefutable pos) =>>> fixPat13 pat
fixPat13 pat  = unitS pat

