module Remove1_3(removeDo,removeExpRecord,removeDecls) where

import Syntax
import State
import IntState
import TokenId(TokenId,t_gtgt,t_gtgteq,t_zero,tTrue)
import TypeLib(getState,newIdent,getIdent,typeError)
import SyntaxPos
import TypeData
import NT
import Kind
import Extra(strPos,dropJust,isJust,isNothing,mixCommaAnd,noPos,dropRight,isRight)
import List
---- Done before Scc

removeDecls (DeclsParse decls) tidFun state =
  case mapS removeDecl decls (ExpCon noPos (tidFun (tTrue,Con))) ([],state) of
    (decls,(zcons,state)) -> (DeclsParse (concat decls),zcons,state)

onePos v 1 (x:xs) = v:xs
onePos v n (x:xs) = x: onePos v (n-1 ::Int) xs

mkFun pos (c,i) =
  r13True >>>= \ true ->
  r13Info c >>>= \ conInfo ->
  r13Unique >>>= \ v ->
  let wildcard = PatWildcard pos
      var = ExpVar pos v
      vars = take (arityI conInfo) (repeat wildcard)  -- arityI safe for constructors :-)
  in
    unitS (Fun [ExpApplication pos (ExpCon pos c:onePos var i vars)] [(true,var)] (DeclsParse []))



r13True    true thread = (true,thread)
r13Info    i down thread@(zcon,state) = (dropJust (lookupIS state i),thread)
r13Unique  down  thread@(zcon,state) = 
  case uniqueIS state of
   (u,state) -> (u,(zcon,state))

remember zcon down thread@(zcons,state) = (zcon:zcons,state)

mkSel (pos,field,selector) = 
  r13Info field >>>= \  (InfoField unique tid icon_offs iData iSel) ->
  mapS (mkFun pos) icon_offs >>>= \ alts ->
  unitS (DeclFun pos selector alts)

removeDecl (DeclConstrs pos zcon cs) =
  remember zcon >>>
  mapS mkSel cs
removeDecl d = unitS [d]

-------- Done after Scc

removeDo [StmtExp exp] = unitS exp
removeDo (StmtExp exp:r) =
  let pos = getPos exp
  in 
    getIdent (t_gtgt,Var) >>>= \ gtgt ->
    removeDo r >>>= \ exp2 ->
    unitS (ExpApplication pos [ExpVar pos gtgt, exp, exp2])
removeDo (StmtLet decls :r) =
  let pos = getPos decls
  in 
    removeDo r >>>= \ exp2 ->
    unitS (ExpLet pos decls exp2)
removeDo (StmtBind pat exp:r) =
  getIdent (t_gtgteq,Var) >>>= \ gtgteq ->
  getState >>>= \ state ->
  removeDo r >>>= \ exp2 ->
  let pos = getPos exp
  in
    if nofail state pat
    then  
      unitS (ExpApplication pos [ExpVar pos gtgteq, exp, ExpLambda pos [pat] exp2])
    else
      getIdent (t_zero,Var) >>>= \ zero ->	-- In H98, this is `fail'
      getIdent (tTrue,Con) >>>= \ true ->
      newIdent >>>= \ x ->
      let eX = ExpVar pos x
	  eTrue = ExpCon pos true
          eFail = ExpApplication pos [ExpVar pos zero
                                     ,ExpLit pos (LitString Boxed "pattern-match failure in do expression")]
      in unitS (ExpApplication pos [ExpVar pos gtgteq
				   ,exp 
				   ,ExpLambda pos [eX] (ExpCase pos eX [Alt pat               [(eTrue,exp2)]  (DeclsScc [])
								       ,Alt (PatWildcard pos) [(eTrue,eFail)] (DeclsScc [])
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


fieldInfo (FieldExp pos field exp) =
  getState >>>= \ state ->
  case lookupIS state field of
    Just (InfoField unique tid icon_offs idata iSel) -> unitS  (idata,(icon_offs,exp))


fixArg given (def,i) =
  case lookup i given of
    Just e -> e
    Nothing -> def

fixAlt pos true exps (con,offsets) =
  getState >>>= \ state ->
  let nargs = [1 .. arityIS state con]
  in 
    mapS ( \ _ -> newIdent) nargs >>>= \ new ->
    let vars = map (ExpVar noPos) new 
        econ = ExpCon pos con
    in unitS
      (Alt (ExpApplication pos (econ:vars))
	   [(true,ExpApplication pos (econ:map  (fixArg (zip offsets exps))
						(zip vars nargs)))]
	   (DeclsScc []))

getOffsets icon_offs con =
  let offsets   = map (\ icon_off -> lookup con icon_off) icon_offs
  in if all isJust offsets
     then Right (con,map dropJust offsets)
     else Left (con,offsets)

removeExpRecord e@(ExpRecord exp' fields') fields = removeExpRecord exp' (fields' ++ fields)
removeExpRecord e@(ExpCon pos con) fields =
  getState >>>= \ state ->
  mapS fieldInfo fields >>>= \ coes ->
      if firstIsEqual coes
      then 
	let (icon_offs,exps) = unzip (map snd coes)
	in case getOffsets icon_offs con of
	    Right (con,offsets) ->
	      unitS (ExpApplication pos (e:map (fixArg (zip offsets exps))
					     (zip (repeat (PatWildcard  pos)) [1 .. arityIS state con]) ))
	    Left (con,offsets) -> typeError (errField1 state pos con offsets fields)
      else typeError (errField2 state fields)
removeExpRecord exp [] =
  typeError (errField4 (getPos exp))
removeExpRecord exp fields =
  getState >>>= \ state ->
  mapS fieldInfo fields >>>= \ coes@((t,_):_) ->
      if firstIsEqual coes
      then
	let (icon_offs,exps) = unzip (map snd coes)
	    pos = getPos exp
	in case (partition isRight . map (getOffsets icon_offs) . constrsI  . dropJust . lookupIS state) t of
	  ([],_) -> typeError (errField3 state fields)
	  (rps,_) ->
	    getIdent (tTrue,Con) >>>= \ true ->
	    mapS (fixAlt pos (ExpCon pos true) exps) (map dropRight rps) >>>= \ alts ->
	    unitS (ExpCase (getPos exp) exp alts)
      else typeError (errField2 state fields)


firstIsEqual [] = True
firstIsEqual ((k,_):kvs) = all (k==) (map fst kvs)  

errField1 state pos con offsets fields =
   "The field(s)" ++ mixCommaAnd (map ( \ (_,FieldExp pos field exp) -> ' ':show (tidIS state field) ++ " at " ++ strPos pos)
				 (filter (isNothing.fst) (zip offsets fields)))
	      ++ " do(es) not belong to constructor " ++ show (tidIS state con) ++ " used at " ++ strPos pos ++ "."

errField2 state fields =
   "The fields" ++ mixCommaAnd (map ( \ (FieldExp pos field exp) -> ' ': show (tidIS state field) ++ " at " ++ strPos pos)
				 fields)
	      ++ " do not belong to the same type."

errField3 state fields =
   "The fields "++ mixCommaAnd (map ( \ (FieldExp pos field exp) -> ' ':show (tidIS state field) ++ " at " ++ strPos pos)
				 fields)
	      ++ " do not belong to the same constructor."

errField4 pos =
   "The update of the expression at " ++ strPos pos ++ " uses an empty list of fields."

