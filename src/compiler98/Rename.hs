{- ---------------------------------------------------------------------------
Renames identifiers (also patches fixity information)

Also provides
ctxs2NT to transform syntax tree type into internal type
fixInstance used by module Derive
-}
module Rename(ctxs2NT, fixInstance, rename) where

import List
import Syntax
import Extra(pair,isJust,dropJust,strace)
import Bind
import RenameLib
import Fixity(fixInfixList)
import IExtract(tvPosTids,freeType,tvTids,countArrows,defFixFun)
import TokenId(TokenId,t_x,t_Tuple,tTrue,t_error,extractV,t_gtgt
              ,t_gtgteq{-,tEval-},t_lessequal,t_subtract)
import State
import IdKind
import Extra
import NT
import IntState
import ImportState(methodsI,uniqueI,tidI)
import AssocTree
import TokenInt
import PackedString(PackedString,packString,unpackPS)
import SyntaxPos
import SyntaxUtil(infixFun)

import StrSyntax(strConstr)
import Overlap(Overlap)

{-
Uniquely rename all identfiers (also patch fixity)
-}
rename :: Flags 
       -> PackedString 
       -> (TokenId -> [TokenId]) 
       -> (Bool -> Bool -> TokenId -> IdKind -> IE) 
       -> [(InfixClass TokenId,Int,[FixId TokenId])] 
       -> Decls TokenId -- declarations of program
       -> ImportState 
       -> Overlap
       -> Either [String] 
            (Decls Int -- renamed declarations of program
            ,IntState  -- internal state with symbol table
            ,(TokenId,IdKind) -> Int       -- tidFun
            ,(TokenId,IdKind) -> Maybe Int -- tidFunSafe
            ,[(Int,[(Pos,Int)])]           -- derived
            ,Maybe [Int]                   -- userDefault
            ,Tree ((TokenId,IdKind),Either [Pos] [Int])  -- rt
            )

rename flags mrps qualFun expFun inf topdecls importState overlap =
  case is2rs flags mrps qualFun expFun overlap importState 
         :: Either [String] (TokenId -> TokenId
                            ,TokenId -> IdKind -> IE
                            ,RenameState
                            ,Tree ((TokenId,IdKind),Either [Pos] [Int])) of
    Right (qualFun,expFun,state,irt) ->
      case renameTopDecls inf topdecls qualFun expFun state
             :: (Decls Int,RenameState) of
	(DeclsParse topdecls,state) -> 
          case keepRS state of
	    (unique,(tidFun,tidFunSafe),rps,ts,derived,userDefaults,[]) ->
 	      case mapS (fixInstance (tidFun (tTrue,Con))) topdecls () 
                     (IntState unique rps ts []) of
		(topdecls,state) -> 
                  Right (DeclsParse topdecls,state,tidFun,tidFunSafe
                        ,derived,userDefaults,irt)
	    (unique,(tidFun,tidFunSafe),rps,st,derived,userDefaults,errors) ->
              Left errors
    Left errors -> Left errors

---- ===============================

{-
In the input of this function every equation is a function declaration of its
own, as produced by the parser. This function groups succeeding equations 
for the same function/variable identifier into a single function declaration.
-}

groupFun :: Eq a => Decls a -> Decls a

groupFun (DeclsParse decls) = DeclsParse (groupFun' decls)
  where

  groupFun' :: Eq a => [Decl a] -> [Decl a]

  groupFun' [] = []
  groupFun' (DeclFun pos fun funs:r) =
    DeclFun pos fun (funs++funs'):    groupFun' r'
    where 
    (funs',r') =     groupFun'' fun [] r
  groupFun' (d@(DeclPat (Alt (ExpVar pos fun) gdexps w)):r) = 
    groupFun' (DeclFun pos fun [Fun [] gdexps w]:r)
  groupFun' (d@(DeclPat (Alt (ExpInfixList pos es) gdexps w)):r) = 
    case infixFun es of
      Nothing -> d: groupFun' r
      Just (e1,pos',fun',e2) -> 
        groupFun' (DeclFun pos' fun' [Fun [e1,e2] gdexps w]:r)
  groupFun' (d:r) = d:    groupFun' r

  groupFun'' :: Eq a => a -> [Fun a] -> [Decl a] -> ([Fun a],[Decl a])

  groupFun'' fun a (DeclFun pos fun' funs:r) | fun == fun' =
    groupFun'' fun (a++funs) r
  groupFun'' fun a  (d@(DeclPat (Alt (ExpVar pos fun') gdexps w)):r) = 
    groupFun'' fun a  (DeclFun pos fun' [Fun [] gdexps w]:r)
  groupFun'' fun a dr@(DeclPat (Alt (ExpInfixList pos es) gdexps w):r) = 
    case infixFun es of
      Nothing -> (a,dr)
      Just (e1,pos',fun',e2) -> 
        groupFun'' fun a  (DeclFun pos' fun' [Fun [e1,e2] gdexps w]:r)
  groupFun'' fun a r = (a,r)


{-
Rename statements of a do expression
-}
renameStmts :: [Stmt TokenId] 
            -> (PackedString -> Int -> TokenId -> TokenId
               ,TokenId -> TokenId
               ,TokenId -> IdKind -> IE
               ,TokenId -> (InfixClass TokenId,Int)
               ) 
            -> RenameState 
            -> ([Stmt Int],RenameState)

renameStmts (StmtExp exp:[]) = renameExp exp >>>= \ exp -> unitS [StmtExp exp]
renameStmts (StmtExp exp:r) =
  renameExp exp >>>= \ exp ->
  renameStmts r >>>= \ r ->
  unitS (StmtExp exp:r)
renameStmts (StmtBind pat exp:r) =
  if null r
  then renameError ("Lambda statement at " ++ strPos (getPos pat) ++ " can not end statement list") [StmtExp (PatWildcard (getPos pat))]
  else renameExp exp >>>= \ exp ->
       pushScope >>>
       bindPat Var pat >>>
       renameExp pat >>>= \ pat ->
       renameStmts r >>>= \ r ->
       unitS (StmtBind pat exp:r) >>>
       popScope
renameStmts (StmtLet decls':r) =
  if null r
  then renameError ("Let statement at " ++ strPos (getPos decls') ++ " can not end statement list") [StmtExp (PatWildcard (getPos decls'))]
  else
    let decls = groupFun decls'
    in pushScope >>>
	bindDecls decls >>>
	renameDecls decls >>>= \ decls ->
	renameStmts r >>>= \ r ->
        unitS (StmtLet decls:r) >>>
       popScope

---- ==============================

renameTopDecls :: [(InfixClass TokenId,Int,[FixId TokenId])] 
               -> Decls TokenId 
               -> (TokenId -> TokenId) 
               -> (TokenId -> IdKind -> IE) 
               -> RenameState 
               -> (Decls Int,RenameState)

renameTopDecls inf topdecls1 qualFun expFun state1 =
 let (DeclsParse topdecls') = groupFun topdecls1
       -- group equations for same function definition together
     fixdecls = sepFixDecls topdecls'
       -- separate fixity declarations
     state2 =  (pushScope >>>
	        bindDecls (DeclsParse topdecls')
		) (globalTid,qualFun,expFun) state1
       -- store all defined term variables and class ids in a memo 

     (fixity,state3) = fixFixityRS defFixFun state2 (inf++fixdecls)
     (topdecls2,state4) =
		(mapS renameDecl topdecls' >>>
		 popScope
		) (globalTid,qualFun,expFun,fixity) state3
 in (DeclsParse topdecls2,state4)


sepFixDecls = concatMap (\decl-> case decl of
                                  DeclFixity f -> [f]
                                  _ -> [])



renameDecls (DeclsParse decls) (_,qualFun,expFun,fixity2) state3 =
    let (fixity3,state4) = fixFixityRS fixity2 state3 (sepFixDecls decls) in
    (unitS DeclsParse =>>> mapS renameDecl decls) (localTid,qualFun,\ _ _ -> IEnone,fixity3) state4


renameDecl :: Decl TokenId 
           -> State (PackedString -> Int -> TokenId -> TokenId
                    ,TokenId -> TokenId
                    ,TokenId -> IdKind -> IE
                    ,TokenId -> (InfixClass TokenId,Int)
                    ) 
                    RenameState (Decl Int) RenameState

renameDecl (DeclType (Simple pos tid tvs) typ) =
  let al = tvPosTids tvs
  in transTypes al (map snd al) [] [typ] >>>= \nt ->
     defineType tid nt >>>  {- = \d -> -}
     --  gross hack...
     unitS (DeclIgnore "Type Synonym")
--   unitS (DeclAnnot (DeclIgnore "Type Synonym") [AnnotArity (pos, d) 0])
renameDecl (DeclDataPrim  pos tid size) =
  uniqueTid pos TCon tid >>>= \ i ->
  defineDataPrim tid (NewType [] [] [] [NTcons i []]) size >>>= \ d ->
  unitS (DeclConstrs pos d [])
renameDecl (DeclData b ctxs (Simple pos tid tvs) constrs posidents) =
  let al = tvPosTids tvs 
      free = map snd al
  in 
     transTypes al free ctxs (map (uncurry TypeVar) tvs ++ [TypeCons pos tid (map (uncurry TypeVar) tvs)]) >>>= \ nt@(NewType free [] ctxs nts) ->
     mapS (renameConstr tid al free ctxs (last nts)) constrs >>>= \csfields ->
     let (cs,noargs,fields) = unzip3 csfields 
     in defineData b tid nt cs >>>= \d ->
        renamePosIdents TCon ({-(pos,tEval):-}posidents) >>>= \ posis ->
        defineDerived d posis >>>
        (if b == Just True && length constrs > 1 && not (and noargs) then
	 renameError ("Unboxed data " ++ show tid ++ " at " ++ strPos pos ++ " is neither an enumeration nor a single constructor data type.")
	else
         unitS) (DeclConstrs pos d (concat fields))

renameDecl (DeclClass pos ctxs tid tvar decls') =
  let al = tvTids [tvar]
      (DeclsParse decls) = groupFun decls'
  in transTypes al (map snd al) ctxs 
       [TypeCons pos tid [TypeVar pos tvar]] >>>= \ nt -> 
     transContext al (Context pos tid (pos,tvar)) >>>= \ ctx@(c,t) -> 
     fixClassMethods tvar ctx decls >>>= \ declmds ->
     defineClass pos tid nt (map snd declmds) >>> 
     unitS (DeclClass pos [] c t (DeclsParse (map fst declmds)))

renameDecl (DeclInstance pos ctxs tid instanceType@(TypeCons _ tcon _)  instmethods') =
  let al = tvTids (snub (freeType instanceType))
      (DeclsParse instmethods) = groupFun instmethods'
  in mapS (renameCtx al) ctxs >>>= \ ctxs -> 
     uniqueTid pos TClass tid >>>= \ c -> 
     renameType al instanceType >>>= \ typ ->
     mapS (renameInstMethod) instmethods >>>= \ ims ->
     unitS (DeclInstance pos ctxs c typ (DeclsParse ims))

renameDecl (DeclDefault types) =
    mapS (renameType []) types >>>= \ types  ->
    defineDefault types >>>
    unitS (DeclIgnore "Type Defaults")


renameDecl (DeclVarsType posidents ctxs typ) =
  let al = (tvTids . snub . freeType) typ
  in unitS DeclVarsType =>>> renamePosIdents Var posidents
			    =>>> mapS (renameCtx al) ctxs
			    =>>> renameType al typ

renameDecl (DeclPat alt) =
    unitS DeclPat =>>> renameDeclAlt alt
renameDecl (DeclFun pos tid funs) =
    unitS (DeclFun pos) =>>> defineVar tid
	                =>>> mapS renameFun funs
renameDecl d@(DeclPrimitive pos tid arity typ) =
  let al = (tvTids . snub . freeType) typ
  in defineVar tid >>>= \ tid ->
     renameType al typ >>>= \ typ ->
     unitS (DeclPrimitive pos tid arity typ)
renameDecl d@(DeclForeignImp pos str tid arity cast typ) =
  let al = (tvTids . snub . freeType) typ
  in defineVar tid >>>= \ tid ->
     renameType al typ >>>= \ typ ->
     unitS (DeclForeignImp pos str tid arity cast typ)
renameDecl d@(DeclForeignExp pos str tid typ) =
  let al = (tvTids . snub . freeType) typ
  in defineVar tid >>>= \ tid ->
     renameType al typ >>>= \ typ ->
     unitS (DeclForeignExp pos str tid typ)

--     Used for unimplemented things
renameDecl d@(DeclIgnore str) = unitS (DeclIgnore str)
renameDecl d@(DeclError str) = unitS (DeclError str)
renameDecl (DeclAnnot decl annots) = error "DeclAnnot"
renameDecl d@(DeclFixity f) = unitS (DeclIgnore "fixity")



---- ========================

fixClassMethods tvar ctx decls =
  case partition isSignature decls of
    (sgn,def) ->
      mapS (renameMethod tvar ctx) sgn >>>= \ ms ->
      mapS renameDefault  (pairDefault (concat ms) def)

renameMethod tvar ctx@(c,tv) (DeclVarsType postids ctxs typ) =
   let al = tvTids (snub (tvar:freeType typ)) -- necessary that type variable for the class contex is the same!
       arity = countArrows typ
   in mapS (transContext al) ctxs >>>= \ ctxs ->
      transType al typ >>>= \ typ ->
      let free = map snd al
          nt = NewType free [] ({-ctx:-}ctxs) [anyNT [head free] typ]   -- The class context is not included in the type
      in mapS ( \ (pos,tid) -> defineMethod pos tid nt arity c >>>= \ m -> unitS (pos,tid,m)) postids

renameDefault (DeclFun pos tid funs,s) =
    defineDefaultMethod  tid >>>= \ i ->
    mapS renameFun funs >>>= \ funs ->
    unitS (DeclFun pos i funs,(s,i))

renameInstMethod  (DeclFun pos tid funs) =
    defineInstMethod  tid >>>= \ i ->
    mapS renameFun funs >>>= \ funs ->
    unitS (DeclFun pos i funs)

isSignature (DeclVarsType posidents ctxs typ) = True
isSignature _ = False

pairDefault ms [] = map mkDMethod ms
pairDefault ms (DeclPat alt:r) = error " Sorry no left hand patterns in classes:-("
pairDefault ms (d@(DeclFun pos tid funs):r) =
   case partition ((tid==).snd3) ms of
     ([],ms) -> strace ("Dropping function " ++ show tid ++ " at " ++ strPos pos ++ " without signature in class.") (pairDefault ms r)
     ([(p,m,i)],ms) -> (d,i) : pairDefault ms r
pairDefault ms (DeclIgnore str:r) = pairDefault ms r

mkDMethod (pos,tid,i) = (DeclFun pos tid [Fun [] (mkNoDefault pos tid) (DeclsParse [])],i)

mkNoDefault pos tid = [(ExpCon pos tTrue,ExpApplication pos [ExpVar pos t_error, ExpLit pos (LitString Boxed ("No default for " ++ show tid))])]


---- =========================

renamePosIdents kind posidents = 
    mapS (renamePosIdent kind)  posidents

renamePosIdent kind (pos,tid) =
    unitS (pair pos) =>>> uniqueTid pos kind tid

renameFun (Fun  pats guardedExps decls') =
 let decls = groupFun decls'
 in pushScope >>>
    	mapS0 (bindPat Var) pats >>>
    	bindDecls decls >>>
    unitS Fun =>>>
	mapS renameExp pats =>>>
	mapS renameGuardedExp guardedExps =>>>
	renameDecls decls >>>
    popScope


renameGuardedExp (guard,exp) =
    unitS pair =>>> renameExp guard =>>> renameExp exp

renameDeclAlt (Alt  pat guardedExps decls') =
  let decls = groupFun decls'
  in mapS (defineVar . snd) (identPat pat) >>>= \ _ -> -- don't need the identifiers here
     pushScope >>>
	bindDecls decls >>>   -- bindPat done earlier
     unitS Alt =>>>
	renameExp pat =>>>
	mapS renameGuardedExp guardedExps =>>>
	renameDecls decls >>>
     popScope

renameCaseAlt (Alt  pat guardedExps decls') =
  let decls = groupFun decls'
  in pushScope >>>
	bindPat Var pat >>>
	bindDecls decls >>>
    unitS Alt =>>>
	renameExp pat =>>>
	mapS renameGuardedExp guardedExps =>>>
	renameDecls decls >>>
    popScope

renameType al (TypeApp t1 t2) =
   unitS TypeApp =>>> renameType al t1 =>>> renameType al t2
renameType al (TypeCons  pos tid types) =
   unitS (TypeCons pos) =>>>   uniqueTid pos TCon tid =>>>   mapS (renameType al) types
renameType al (TypeVar   pos tid)    =
   unitS (TypeVar pos) =>>> uniqueTVar pos al tid

renameCtx al (Context pos tid (p,t)) =
    uniqueTid pos TClass tid >>>= \ i ->
    uniqueTVar p al t >>>= \ t ->
    unitS (Context pos i (p,t))

renameConstr typtid al free ctxs resType@(NTcons bt _) c@(Constr pos tid fieldtypes) =
  let e =  [] -- no forall if Constr is used
      es = zip e [1 + length al .. ]
  in
    mapS (transFieldType (es++al)) fieldtypes >>>= \ntss ->
    let all = concat ntss
	nts = map snd all
	ifs = map ( ( \ v -> case v of Just (p,tid,i) -> Just i; _ -> Nothing) . fst) all
	exist = map snd es
    in
      defineConstr tid (NewType (map snd al ++ exist) exist ctxs (nts++[resType])) ifs bt >>>= \ c ->
      mapS (defineField typtid bt c) (zip all [ 1:: Int ..]) >>>= \ fs ->
      unitS (c,null nts,map dropJust (filter isJust fs))

renameConstr typtid al free ctxs resType@(NTcons bt _) (ConstrCtx forall ectxs' pos tid fieldtypes) =
  let ce = map ( \( Context _ _ (_,v)) -> v) ectxs'
      e =  map snd forall -- filter (`notElem` (map fst al)) $ snub $  (ce ++) $ concat $ map (freeType . snd) fieldtypes
      es = zip e [1 + length al .. ]
  in
    mapS (transFieldType (es++al)) fieldtypes >>>= \ntss ->
    let all = concat ntss
        nts = map snd all
        ifs = map ( ( \ v -> case v of Just (p,tid,i) -> Just i; _ -> Nothing) . fst) all
	exist = map snd es
    in
      mapS (transContext (es++al)) ectxs' >>>= \ ectxs ->
      defineConstr tid (NewType (map snd al ++ exist) exist ctxs  (map ( \ (c,v) -> NTcontext c v) ectxs ++ nts++[resType])) ifs bt >>>= \ c ->
      mapS (defineField typtid bt c) (zip all [ 1:: Int ..]) >>>= \ fs ->
      unitS (c,null nts,map dropJust (filter isJust fs))


transFieldType al (Nothing,typ) =				-- nearly identical to transFieldType in IExtract!!!
  transType al typ >>>= \ typ -> unitS [(Nothing,typ)]
transFieldType al (Just posidents,typ) =
  transType al typ >>>= \ typ ->
  mapS ( \ (p,v) -> uniqueTid p Field v >>>= \ i -> unitS (Just (p,v,i),typ))  posidents

renameField (FieldExp pos tid exp) = unitS (FieldExp pos) =>>> uniqueTid pos Field tid =>>> renameExp exp
renameField (FieldPun pos tid) = checkPuns pos >>> unitS (FieldExp pos) =>>> uniqueTid pos Field tid =>>> (unitS (ExpVar pos) =>>> uniqueTid pos Var tid)


renameExp :: Exp TokenId 
          -> State ( PackedString -> Int -> TokenId -> TokenId
                   , TokenId -> TokenId
                   , TokenId -> IdKind -> IE
                   , TokenId -> (InfixClass TokenId,Int)) 
                   RenameState (Exp Int) RenameState

renameExp (ExpScc            str exp) = unitS (ExpScc str) =>>> renameExp exp
renameExp (ExpLambda         pos pats exp) =
    pushScope >>>
	mapS0 (bindPat Var) pats >>>
    unitS (ExpLambda pos) =>>>
	mapS renameExp pats =>>>
	renameExp exp >>>
    popScope
renameExp (ExpDo pos stmts) = unitS (ExpDo pos) =>>> renameStmts stmts
renameExp (ExpRecord exp fields) = unitS ExpRecord =>>> renameExp exp =>>> mapS renameField fields
renameExp (ExpLet            pos decls' exp) =
  let decls = groupFun decls'
  in pushScope >>>
	bindDecls decls >>>
     unitS (ExpLet pos) =>>>
	renameDecls decls =>>>
	renameExp exp >>>
     popScope
renameExp (ExpCase           pos exp alts) =
    unitS (ExpCase pos) =>>> renameExp exp =>>> mapS renameCaseAlt alts
renameExp (ExpIf             pos expCond expThen expElse) =
    unitS (ExpIf pos) =>>> renameExp expCond =>>> renameExp expThen =>>> renameExp expElse
renameExp (ExpType           pos exp ctxs typ) =
    let al = (tvTids . snub . freeType) typ
    in renameExp exp >>>= \ exp ->  
       mapS (renameCtx al) ctxs >>>= \ ctxs ->
       renameType al typ >>>= \ typ -> 
       unitS (ExpType pos exp ctxs typ)
--- Above only in expressions
renameExp (ExpApplication   pos exps)  =
    unitS (ExpApplication pos) =>>> mapS renameExp exps

renameExp (ExpInfixList     pos exps)  = 
    fixInfixList exps >>>= \ exp -> renameExp exp
renameExp (ExpVar           pos tid) =
    unitS (ExpVar pos) =>>> uniqueTid pos Var tid
renameExp (ExpCon           pos tid) = 
    unitS (ExpCon pos) =>>> uniqueTid pos Con tid
renameExp (ExpVarOp         pos tid) = 
    unitS (ExpVarOp pos) =>>> uniqueTid pos Var tid
renameExp (ExpConOp         pos tid) = 
    unitS (ExpConOp pos) =>>> uniqueTid pos Con tid
renameExp e@(ExpLit         pos lit)   = unitS (ExpLit         pos lit)
--renameExp (ExpTuple         pos exps)  =
--    unitS (ExpTuple pos) =>>> mapS renameExp exps
renameExp (ExpList          pos exps)  = 
    unitS (ExpList pos) =>>> mapS renameExp exps
--- Below only in patterns
renameExp (PatAs            pos tid pat) =
    unitS (PatAs pos) =>>> uniqueTid pos Var tid =>>> renameExp pat
renameExp e@(PatWildcard      pos) =
    unitS (PatWildcard      pos)
renameExp (PatIrrefutable    pos pat) =
    unitS (PatIrrefutable pos) =>>> renameExp pat
renameExp (PatNplusK        pos tid _ k _ _) =
    bindNK pos >>>= \ tid' ->
    let leq = ExpVar pos t_lessequal
        sub = ExpVar pos t_subtract
        n'  = ExpVar pos tid'
        n   = ExpVar pos tid 
    in
    unitS (PatNplusK pos) =>>>
      uniqueTid pos Var tid =>>>
      uniqueTid pos Var tid' =>>>
      renameExp k =>>>
      renameExp (ExpApplication pos [leq,k,n']) =>>>
      renameExp (ExpApplication pos [sub,k,n'])




----- ===================

fixInstance :: Int -> Decl Int -> a -> IntState -> (Decl Int,IntState)

fixInstance iTrue (DeclInstance pos ctxs i instanceType@(TypeCons _ ti tvs) 
  (DeclsParse instmethods)) =
    ensureDefaults pos i >>>= \ cinfo ->
    mapS ( \ (m,d) -> getInfo m >>>= \ minfo -> unitS (minfo,d)) (methodsI cinfo) >>>= \ cmds ->
    getInfo ti >>>= \ tinfo ->
    mapS ( \ (pos,i) -> getInfo i >>>= \ info -> unitS (pos,info)) (map getI instmethods) >>>= \ ims ->
    let
      free = map ( \ (TypeVar _ v) -> v) tvs
      ctxsNT =  ctxs2NT ctxs
      nt = NewType free [] ctxsNT [NTcons ti (map NTvar free)]
      cmds' = map ( \ (info,d) -> (extractV (tidI info),(info,d))) cmds
      old = map ( \ (pos,info) -> (pos,extractV (tidI info),info)) ims      
      dms = map snd3 old
      (err,upd) = ( partition isLeft
	          . map ( \ (pos,rps,i) ->
				 case lookup rps cmds' of
		  		   Just (si,d) -> Right (uniqueI i,uniqueI si)
				   Nothing     -> Left (pos,rps))
		  ) old

      tidtyp = tidI tinfo
      tidcls = tidI cinfo
    in
      instanceError (show tidcls) err >>>
      addInstance i ti free ctxsNT >>>
      mapS (mkIMethod pos tidcls tidtyp iTrue nt) (filter ((`notElem` dms).fst) cmds') >>>= \ fill ->
      mapS0 ( ( \ ( im, m) -> updInstMethodNT tidcls tidtyp im nt m) . dropRight) upd >>> 
      unitS (DeclInstance pos ctxs i instanceType (DeclsParse (fill++instmethods)))
fixInstance iTrue d = unitS d

instanceError cstr [] = unitS0
instanceError cstr (Left (pos,rps):xs) =
  (\ down state ->
      addError state
      ("The identifier " ++ reverse (unpackPS rps) ++ " instansiated at " ++ strPos pos ++ " does not belong to the class " ++ cstr ++ ".")) >>>
  instanceError cstr xs

mkIMethod pos tidcls tidtyp iTrue nt (rpsid,(minfo,d)) =
  let uniqueM = uniqueI minfo
  in addInstMethod tidcls tidtyp (tidI minfo) nt uniqueM >>>= \ mi ->
     unitS (DeclFun pos mi [Fun [] [(ExpCon pos iTrue,ExpVar pos d)] (DeclsParse [])])

getI (DeclFun pos i funs) = (pos,i)

ctxs2NT ctxs = map ctx2NT ctxs
 where
   ctx2NT (Context pos c (p,v)) = (c,v)


ensureDefaults pos i down state =
  case lookupIS state i of
    Just info@(InfoClass u tid e nt ms ds inst) ->
      if length ms == length ds
      then (info,state)
      else
	case uniqueISs state ms of
	  (mds,state) ->
	    let newInfo = InfoClass u tid e nt ms (map snd mds) inst
	    in (newInfo
	       ,foldr (addDefaultMethod tid) 
                      (updateIS state i ( \ _ -> newInfo))
		      mds
	       )


------ ===================





