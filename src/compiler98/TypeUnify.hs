module TypeUnify(unify,unifyr) where

import NT(NT(..),NewType(..),freeNT,strNT)
import IdKind
import TypeSubst
import TypeUtil
import TypeData
import TokenId(TokenId(..))
import PackedString(PackedString)
import Info
import IntState

unify state phi (t1@(NTany tvn1),t2) =
  case applySubst phi tvn1 of
    Nothing     -> extend phi tvn1 (subst phi t2)
    Just phitvn -> unify state phi (phitvn,subst phi t2)

unify state phi (t1@(NTvar tvn1),(NTany tvn2)) =
  case applySubst phi tvn2 of
    Nothing     -> extend phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTvar tvn1),t2) =
  case applySubst phi tvn1 of
    Nothing     -> extendV state phi tvn1 (subst phi t2)
    Just phitvn -> unify state phi (phitvn,subst phi t2)

unify state phi (t1@(NTcons _ _),t2@(NTany tvn2)) =
  case applySubst phi tvn2 of
    Nothing     -> extend phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTcons _ _),t2@(NTvar tvn2)) =
  case applySubst phi tvn2 of
    Nothing     -> extendV state phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTcons c1 ts1),t2@(NTcons c2 ts2)) =
  if c1 == c2 
  then if length ts1 == length ts2 -- length check because of constructor classes
       then unifyl state phi (zip ts1 ts2)
       else
         case lookupIS state c1 of
           Just info ->
             Left (phi, "can not unify " ++ show (tidI info) ++ " with " ++ show (length ts1) ++ " and " ++ show (length ts2) ++ " arguments")
  else
    case (unifyExpand state c1,unifyExpand state c2) of
      (Left s1       ,Left s2       )          -> Left (phi,"type clash between " ++ s1 ++ " and " ++ s2)
      (Left  _       ,Right (d2,nt2))          -> unify state phi (t1            ,expand nt2 ts2)
      (Right (d1,nt1),Left _        )          -> unify state phi (expand nt1 ts1,t2            )
      (Right (d1,nt1),Right (d2,nt2)) | d2<=d1 -> unify state phi (expand nt1 ts1,t2            )
      (Right (d1,nt1),Right (d2,nt2))          -> unify state phi (t1            ,expand nt2 ts2)

unify state phi (t1@(NTcons c1 ts1),t2@(NTapp ta2 tb2)) =
  case expandAll state t1 of
    t1@(NTcons c1 ts1) ->
--      strace ("unify(1) " ++ show t1 ++ " " ++ show t2) $
      case ts1 of
        [] ->
          case lookupIS state c1 of
            Just info ->
              Left (phi, "type clash between type applicationa and " ++ show (tidI info))
        _ ->  unifyl state phi [(NTcons c1 (init ts1),ta2),(last ts1,tb2)]


unify state phi (t1@(NTapp ta1 tb1),t2@(NTany tvn2)) =
--  strace ("unify(2) " ++ show t1 ++ " " ++ show t2) $
  case applySubst phi tvn2 of
    Nothing     -> extend phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTapp ta1 tb1),t2@(NTvar tvn2)) =
--  strace ("unify(3) " ++ show t1 ++ " " ++ show t2) $
  case applySubst phi tvn2 of
    Nothing     -> extend phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTapp ta1 tb1),t2@(NTcons c2 ts2)) =
  case expandAll state t2 of
    t2@(NTcons c2 ts2) ->
--      strace ("unify(4) " ++ show t1 ++ " " ++ show t2) $
      case ts2 of
	[] ->
	  case lookupIS state c2 of
	    Just info ->
	      Left (phi, "type clash between " ++ show (tidI info) ++ " and type application ")
	_ -> unifyl state phi [(ta1,NTcons c2 (init ts2)),(tb1,last ts2)]

unify state phi (t1@(NTapp ta1 tb1),t2@(NTapp ta2 tb2)) =
--  strace ("unify(5) " ++ show t1 ++ " " ++ show t2) $
  unifyl state phi [(ta1,ta2),(tb1,tb2)]

unify state phi (t1@(NTexist tvn1),(NTexist tvn2)) =
  if tvn1 == tvn2 then
    Right phi
  else
   Left (phi,"type clash between existential types")

unify state phi ((NTexist _),(NTcons c _)) =
  case lookupIS state c of
    Just info ->
      Left (phi, "type clash between " ++ show (tidI info) ++ " and existential type ")

unify state phi ((NTcons c _),(NTexist _)) =
  case lookupIS state c of
    Just info ->
      Left (phi, "type clash between " ++ show (tidI info) ++ " and existential type ")

unify state phi ((NTexist _),(NTapp _ _)) =
   Left (phi,"type clash between existential type and type application")

unify state phi ((NTapp _ _),(NTexist _)) =
   Left (phi,"type clash between existential type and type application")

unify state phi (t1@(NTexist e),t2@(NTany tvn2)) =
-- strace ("unify exist " ++ show e ++ " any " ++ show tvn2) $ 
  case applySubst phi tvn2 of
    Nothing     -> extend phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi (t1@(NTexist e),t2@(NTvar tvn2)) =
-- strace ("unify exist " ++ show e ++ " var " ++ show tvn2) $ 
  case applySubst phi tvn2 of
    Nothing     -> extendV state phi tvn2 (subst phi t1)
    Just phitvn -> unify state phi (phitvn,subst phi t1)

unify state phi _ = error "unify phi _"


unifyl state phi eqns = foldr unify' (Right phi) eqns
        where
            unify' eqn (Right phi) = unify state  phi eqn
            unify' eqn (Left err) = Left err


unifyr state phi [] = Right phi
unifyr state phi [e] = Right phi
unifyr state phi (e1:e2:es) =
       case unify state  phi (e1,e2) of
          Left err -> Left err
          Right phi' -> unifyr state phi' (e1:es)


------

expandAll state t@(NTcons tcon ts) =
  case unifyExpand state tcon of
    Left _ -> t
    Right (d,nt) -> expandAll state (expand nt ts)
expandAll state t = t

unifyExpand state tcon =
    case lookupIS state tcon of
      Just info ->
        case depthI info of
          Just d -> Right (d,ntI info)
	  Nothing -> Left (show (tidI info))

expand (NewType free [] ctxs [nt]) ts = subst (list2Subst (zip free ts)) nt

extendV state phi tvn t@(NTcons c _) =
  if unboxedIS state c then
   Left (phi,"polymorphic type variable bound to unboxed data " ++ strIS state c)
  else
   extend phi tvn t
extendV state phi tvn t = 
 extend phi tvn t

extend phi tvn t@(NTany tvn') =
  if tvn' == tvn
  then Right phi
  else Right (addSubst phi tvn  t)
extend phi tvn t@(NTvar tvn') =
  if tvn' == tvn
  then Right phi
  else Right (addSubst phi tvn  t)
extend phi tvn t | tvn `elem` freeNT t = Left (phi,"occurence")
extend phi tvn t@(NTcons c _) =
   
   Right (addSubst phi tvn t)
extend phi tvn t = Right (addSubst phi tvn t)
