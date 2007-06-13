module TypeCtx( buildCtx, buildDefaults, ctxsReduce, ctxsSimplify, initCtxs) where

import List(sort)
import NT
import IntState
import Info
import TypeSubst
import Extra
import AssocTree
import Syntax
import TypeData
import State

initCtxs = []

removeTSyn state nt@(NTstrict nt') = removeTSyn state nt'
removeTSyn state nt@(NTcons c _ nts) =
  case lookupIS state c of
    Just (InfoData u tid exp (NewType free [] _ [nt])
                             (DataTypeSynonym uboxed depth)) ->
	 -- No context in type synonyms
	 removeTSyn state (substNT (zip free nts) nt)
    _ -> -- It must be an InfoData here, or we have an internal error
         nt
removeTSyn state tvar = tvar



-- Is c a superclass of cstart?
scof :: IntState -> Int -> Int -> Bool
scof state c cstart =
  case lookupIS state cstart of
    Just info -> let sc = superclassesI info
                 in any (c==) sc || any (scof state c) sc


-- ctxsReduce only works on NTvar and NTexist
ctxsReduce ::  IntState -> [(Int,NT)] -> [(Int,NT)]
ctxsReduce state ctxs =
  case foldr (ctxReduce state) [] ctxs of
   ctxs ->  foldr (ctxReduce state) [] (reverse ctxs)   -- Not very nice but...

ctxReduce ::  IntState -> (Int,NT) -> [(Int,NT)] -> [(Int,NT)]
ctxReduce state ctx@(c,nt) ctxs =
  let v = stripNT nt
  in if ctx `elem` ctxs then ctxs
     else let sametvar = filter ((v==) . stripNT . snd) ctxs
          in if (any (scof state c) . map fst) sametvar
	     then ctxs
             else ctx: ctxs

ctxsSimplify :: [Pos] -> IntState -> [((Int,Int),([Int],[(Int,Int)]))]
                -> TypeDict -> [(Int,NT)]
ctxsSimplify poss state given cls_nt =
  ctxsSimplify' poss state given cls_nt []

-- Only NTvar and NTexist in result
ctxsSimplify' :: [Pos] -> IntState -> [((Int,Int),([Int],[(Int,Int)]))]
                 -> TypeDict -> [(Int,NT)] -> [(Int,NT)]
ctxsSimplify' _ state given (TypeDict cls (NTany v) ipos) r = (cls,mkNTvar v):r
ctxsSimplify' _ state given (TypeDict cls (NTvar v k) ipos) r = (cls,NTvar v k):r
ctxsSimplify' _ state given (TypeDict cls (NTexist v k) ipos) r = (cls,NTexist v k):r
ctxsSimplify' poss state given (TypeDict cls (NTstrict nt) ipos) r =
  -- Don't keep strictness information in ctx?
  ctxsSimplify' poss state given (TypeDict cls nt ipos) r
ctxsSimplify' poss state given (TypeDict cls nt ipos) r =
  case removeTSyn state nt of
    (NTvar v k) ->  (cls,NTvar v k):r
    (NTany v)   ->  (cls,mkNTvar v):r
    (NTexist v k) ->  (cls,NTexist v k):r
    (NTstrict nt) ->   -- Don't keep strictness information in ctx?
      ctxsSimplify' poss state given (TypeDict cls nt ipos) r
    (NTcons con _ nts) ->
      case lookup (cls,con) given of
        Just (tvs,ctxs) -> -- A derived instance
	  foldr (ctxsSimplify' poss state given) r (pair2ctxs ipos tvs nts ctxs)
	Nothing ->
	  case lookupIS state cls of
	    Nothing -> error ("Internal: CtxsSimplify couldn't find the class "
                              ++ show cls)
	    Just info -> 
	      case lookupAT (instancesI info) con of
		Just (tvs,ctxs) ->  
		  foldr (ctxsSimplify' poss state given) r
                        (pair2ctxs ipos tvs nts ctxs)
		Nothing -> error ("The class " ++ strIS state cls ++
				 " has no instance for the type "
				 ++ strIS state con
				 ++ ".\nPossible sources for the problem are: "
				 ++ mixCommaAnd (map (strPos . snd) ipos)
                                 ++ "\nWhen type checking declarations at: " 
                                 ++ mixCommaAnd (map strPos poss)
                                 ++ "\n")
--  (NTapp (NTvar v k) nt2) -> 
--              (cls,NTapp (NTvar v k) nt2):r
--  (NTapp (NTany v) nt2) -> 
--              (cls,NTapp (mkNTvar v) nt2):r
    (NTapp nt1 nt2) -> 
        error ("Couldn't simplify the context (" ++ strIS state cls ++ " ("
                ++ strNT (strIS state) strTVar nt1 ++ " "
                ++ strNT (strIS state) strTVar nt2
                ++ ")).\nPossible sources for the problem are: "
                ++ mixCommaAnd (map (strPos . snd) ipos))
    ent -> error ("Internal: CtxsSimplify expanded the type synonym "
                ++ show nt ++ " to " ++ show ent 
		++ "\nInternal: expected a type constructor")



pair2ctxs ipos tvs nts ctxs =
  let al = zip tvs nts
  in map ( \ (c,v) -> TypeDict c  (dropJust (lookup v al)) ipos) ctxs

--- ===================================

isVar (NTvar v _) = True
isVar (NTexist v _) = True
isVar _ = False

buildCtx :: IntState -> Pos -> [((Int, NT), Int)] -> TypeDict -> Exp Int
buildCtx state pos given (TypeDict cls (NTany tvar) ipos)=
  buildCtx state pos given (TypeDict cls (mkNTvar tvar) ipos)
buildCtx state pos given (TypeDict cls nt ipos) | isVar nt =
  case lookup (cls,nt) given of
    Just i -> ExpVar pos i
    Nothing ->
      let lpis =
		( sort
		. map ( \ ((p,i):_) -> (length p,p,i) )
		. filter (not.null)
		. map ( \ ((c,ntv),i) -> ( map ( \ (c,p) -> (c:p,i) )
					 . filter ((cls==).fst)
					 . allSCof state
					 ) c)
		. filter ((nt==).snd.fst)
		) (given::[((Int,NT),Int)])
      in case lpis of
	((_,p,i):_) -> mkPath state pos (ExpVar pos i) (reverse p)
        [] -> -- Error message generated elsewhere, probably when deriving need
              (PatWildcard pos)
buildCtx state pos given (TypeDict cls nt ipos) =
      case removeTSyn state nt of
	nt@(NTcons con _ nts) ->
	  case lookupIS state cls of
	    Just info -> 
	      case lookupAT (instancesI info) con of
		Just (tvs,[]) ->  
		   mkRealCon pos state cls con
		Just (tvs,ctxs) ->  
		  ExpApplication pos (mkRealCon pos state cls con
                                     : map (buildCtx state pos given)
                                           (pair2ctxs ipos tvs nts ctxs))
		Nothing -> -- Error message generated elsewhere,
                           -- probably when deriving need
                           (PatWildcard pos)
	nt ->
	  buildCtx state pos given (TypeDict cls nt ipos)

mkRealCon pos state cls con = Exp2 pos cls con
{- Not used since March'96 version of Haskell 1.3
 case lookupIS state con of
   Just conInfo ->
     if isRealData conInfo
     then Exp2 pos cls con
     else mkRealCon state cls (getIndDataIS state conInfo)
-}

mkPath state pos ea (f:t:r) =
  -- superclass from class
  mkPath state pos (ExpApplication pos [Exp2 pos f t,ea]) (t:r)
mkPath state pos ea _       = ea

--     get all super classes of c (including c itself!) in width first order
-- allSCof :: IntState -> Int ->  [(Int,[Int])]
allSCof state c = allSCof' state [(c,[])]

-- allSCof' :: IntState -> [(Int,[Int])] ->  [(Int,[Int])]
allSCof' state [] = []
allSCof' state (cp@(c,p):cs) =
  case lookupIS state c of
    Just info -> let sc = (map ( \ s -> (s,c:p)) . superclassesI) info
                 in cp : allSCof' state (cs++sc)


-- Default does not work if it creates new dependencies,
-- this brutal hack cannot handle arguments either!
-- findDefault :: [AssocTree Int ([b],[c])] -> [Int] -> Maybe Int
findDefault insts [] = Nothing
findDefault insts (d:ds) =
  if all (\inst-> case lookupAT inst d of Just ([],[])-> True; _-> False) insts
  then Just d
  else findDefault insts ds

-- oneDefault :: (Int,[(Int,Int)]) -> (Pos,Exp Int,[Int])
--               -> IntState -> ([Decl Int],IntState)
oneDefault (tvar,cis) (pos,trueExp,defaults) state =
  case  findDefault (map (instancesI . dropJust . lookupIS state . fst) cis)
                    defaults of
    Just con -> (map (\(cls,i)->
                        (DeclFun noPos i
                           [Fun [] (Unguarded (mkRealCon pos state cls con))
                                   (DeclsScc [])])) cis
                ,state)
    Nothing -> ([]
               ,addError state ("No default for "
                                ++ concatMap ((' ':).strIS state . fst) cis
                                ++ " at " ++ strPos pos ++ "." 
				++ "(" ++ show tvar ++ "," ++ show cis++")"))

-- buildDefaults :: Pos -> [((Int,NT),Int)] -> Exp Int -> [Int] -> IntState
--                  -> ([(Decl Int)],IntState)
buildDefaults pos defaultCtxsi trueExp defaults state =
  let setup = listAT (foldr (\((c,nt),i) t -> addAT t (++) (stripNT nt)
                                                    [(c,i)])
                            initAT defaultCtxsi)
      (defaultDecls,state') = mapS oneDefault setup (pos,trueExp,defaults) state
  in (concat defaultDecls,state')