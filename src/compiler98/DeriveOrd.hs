module DeriveOrd(deriveOrd) where

import Syntax
import IntState
import Kind
import NT
import State
import DeriveLib
import TokenId(TokenId,t_fromEnum,tFalse,tTrue,tOrd,t_equalequal,t_lessthan,t_lessequal,tcompare,tLT,tEQ,tGT,t_andand,t_pipepipe)

deriveOrd tidFun cls typ tvs ctxs pos =
 getUnique >>>= \x ->
 getUnique >>>= \y ->
 getUnique >>>= \z ->
 getUnique >>>= \w ->
 let expX = ExpVar pos x
     expY = ExpVar pos y
     expZ = ExpVar pos z
     expW = ExpVar pos w
     iLessEqual = tidFun (t_lessequal,Method)
     expLessEqual = ExpVar pos iLessEqual
     iCompare = tidFun (tcompare,Method)
     expCompare = ExpVar pos iCompare
     expTrue = ExpCon pos (tidFun (tTrue,Con))
     exp_fromEnum = ExpVar pos (tidFun (t_fromEnum,Var))
 in
  getInfo typ >>>= \ typInfo -> 
  mapS getInfo (constrsI typInfo) >>>= \ constrInfos ->
  addInstMethod tOrd (tidI typInfo) t_lessequal (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iLessEqual >>>= \ funle ->
  addInstMethod tOrd (tidI typInfo) tcompare (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iCompare >>>= \ funcompare ->
  if all noArgs constrInfos
  then unitS $
	 DeclInstance pos (syntaxCtxs pos ctxs) cls (syntaxType pos typ tvs) $
	    DeclsParse [DeclFun pos funle 
			[Fun [expX,expY]
			     [(expTrue,ExpApplication pos [expLessEqual,ExpApplication pos [exp_fromEnum,expX],ExpApplication pos [exp_fromEnum,expY]])]
			     (DeclsParse [])]
		       ,DeclFun pos funcompare
			[Fun [expZ,expW]
			     [(expTrue,ExpApplication pos [expCompare,ExpApplication pos [exp_fromEnum,expZ],ExpApplication pos [exp_fromEnum,expW]])]
			     (DeclsParse [])]
		       ]
  else
   let expLess = ExpVar pos (tidFun (t_lessthan,Method))
       expEqual = ExpVar pos (tidFun (t_equalequal,Method))
       expLT = ExpCon pos (tidFun (tLT,Con))
       expEQ = ExpCon pos (tidFun (tEQ,Con))
       expGT = ExpCon pos (tidFun (tGT,Con))


   in  mapS (mkOrdFunLe expTrue expLessEqual expLess expEqual tidFun pos) constrInfos >>>= \ funles ->
       mapS (mkOrdFunCompare expTrue expCompare expLT expEQ expGT tidFun pos) constrInfos >>>= \ funcompares ->
       unitS $
	  DeclInstance pos (syntaxCtxs pos ctxs) cls (syntaxType pos typ tvs) $
	      DeclsParse [DeclFun pos funle (funles++
					   [Fun [expX,expY]
					        [(expTrue,ExpApplication pos [expLessEqual,ExpApplication pos [exp_fromEnum,expX],ExpApplication pos [exp_fromEnum,expY]])]
					        (DeclsParse [])])
			 ,DeclFun pos funcompare (funcompares++
					   [Fun [expZ,expW]
					        [(expTrue,ExpApplication pos [expCompare,ExpApplication pos [exp_fromEnum,expZ],ExpApplication pos [exp_fromEnum,expW]])]
					        (DeclsParse [])])

			]


mkOrdFunLe expTrue expLessEqual expLess expEqual tidFun pos constrInfo =
 let con = ExpCon pos (uniqueI constrInfo)
 in case ntI constrInfo of
     NewType _ _ _ [nt] -> -- This constructor has no arguments
        unitS (Fun [ExpApplication pos [con],ExpApplication pos [con]] [(expTrue,expTrue)] (DeclsParse []))
     NewType _ _ _ (_:nts) ->  -- We only want a list with one element for each argument, the elements themselves are never used
      mapS ( \ _ ->
	     getUnique >>>= \ x ->
	     getUnique >>>= \ y -> 
             unitS (ExpVar pos x,ExpVar pos y))
             nts >>>= \ (v@(l,r):vars) ->
      let (lvs,rvs) = unzip vars
	  expAnd = ExpVar pos (tidFun (t_andand,Var))
	  expOr = ExpVar pos (tidFun (t_pipepipe,Var))
      in  
        unitS (
	    Fun [ExpApplication pos (con:lvs++[l]),ExpApplication pos (con:rvs++[r])]
	    [(expTrue,foldr ( \ (v,r) e -> ExpApplication pos [expOr,ExpApplication pos [expLess,v,r],ExpApplication pos [expAnd,ExpApplication pos [expEqual,v,r],e]])
			    (ExpApplication pos [expLessEqual,l,r])
			    vars)]
	    (DeclsParse [])
        )


mkOrdFunCompare expTrue expCompare expLT expEQ expGT tidFun pos constrInfo =
 let con = ExpCon pos (uniqueI constrInfo)
 in case ntI constrInfo of
     NewType _ _ _ [nt] -> -- This constructor has no arguments
        unitS (Fun [ExpApplication pos [con],ExpApplication pos [con]] [(expTrue,expEQ)] (DeclsParse []))
     NewType _ _ _ (_:nts) ->  -- We only want a list with one element for each argument, the elements themselves are never used
      mapS ( \ _ ->
	     getUnique >>>= \ x ->
	     getUnique >>>= \ y -> 
             unitS (ExpVar pos x,ExpVar pos y))
             nts >>>= \ (v@(l,r):vars) ->
      let (lvs,rvs) = unzip vars
	  expAnd = ExpVar pos (tidFun (t_andand,Var))
	  expOr = ExpVar pos (tidFun (t_pipepipe,Var))
      in  
        unitS (
	    Fun [ExpApplication pos (con:lvs++[l]),ExpApplication pos (con:rvs++[r])]
	    [(expTrue,foldr ( \ (v,r) e -> ExpCase pos (ExpApplication pos [expCompare,v,r])
						[Alt  expLT [(expTrue,expLT)] (DeclsParse [])
						,Alt  expEQ [(expTrue,e)] (DeclsParse [])
						,Alt  expGT [(expTrue,expGT)] (DeclsParse [])
						])
			    (ExpApplication pos [expCompare,l,r])
			    vars)]
	    (DeclsParse [])
        )

