module TypeSubst(Substitute(..),idSubst,AssocTree(..),Tree,substEnv,substCtxs,stripSubst,addSubst,applySubst,list2Subst,strace,substNT) where 

import NT(NT(..),NewType(..),freeNT)
import Extra(fstOf,dropJust,strace)
import TypeData
import AssocTree
import Tree234

forceList [] c = c
forceList (x:xs) c = seq x (forceList xs c)

idSubst = initAT

class Substitute a where
  subst :: AssocTree Int NT -> a -> a

instance Substitute NT where
  subst phi nt@(NTany v)    =
    case lookupAT phi v of
      Just nt -> subst phi nt
      Nothing -> nt
  subst phi nt@(NTvar v)    =
    case lookupAT phi v of
      Just nt -> subst phi nt
      Nothing -> nt
  subst phi nt@(NTexist v)    =
      nt
  subst phi (NTcons con ts) =
      let ts' = map (subst phi) ts
      in forceList ts' (NTcons con ts')
  subst phi (NTapp t1 t2) =
     let t2' = subst phi t2
     in seq t2'
        (case subst phi t1 of
	  NTcons con ts ->
            (NTcons con (ts ++ [t2']))
          t -> NTapp t t2'
        )
  subst phi nt@(NTcontext con v) = nt

instance Substitute NewType where
  subst phi (NewType free [] ctxs ts) =
    let ts' = map (subst phi) ts
    in forceList ts' (NewType free [] ctxs ts')

instance Substitute  TypeDict where
  subst phi (TypeDict c nt ip) =
    let nt' = subst phi nt
    in seq nt' (TypeDict c nt' ip)

substEnv phi env = map ( \ (e,nt) -> (e,subst phi nt)) env
substCtxs phi ctxs = map (subst phi) ctxs

applySubst phi tvar =
  case lookupAT phi tvar of
    Just nt@(NTvar tvar) -> applySubst' phi nt tvar
    Just nt@(NTany tvar) -> applySubst' phi nt tvar
    Just nt -> Just nt
    Nothing -> Nothing
 where 
   applySubst' phi nt tvar =
     case lookupAT phi tvar of
       Just nt@(NTvar tvar) -> applySubst' phi nt tvar
       Just nt@(NTany tvar) -> applySubst' phi nt tvar
       Just nt -> Just nt
       Nothing -> Just nt


-- stripSubst :: AssocTree Int NT -> Int -> Tree (Int,NT)
stripSubst phi tvar = phi -- nhc98 doesn't strip substitutions

-- addSubst :: AssocTree Int NT -> Int -> NT -> AssocTree Int NT
addSubst phi tvar t =
   addAT phi (\ a b -> error ("Two mappings for " ++ show tvar ++ " : " ++ show a ++ " and " ++ show b)) tvar t


-- list2Subst :: [(Int,NT)] -> AssocTree Int NT
list2Subst xs = foldr ( \ (v,nt) phi -> addSubst phi v nt) idSubst xs

--- substNT only goes one step, used for (1->2),(2->1) substitutions in TypeCtx

substNT tv (NTany  a) = dropJust (lookup a tv)
substNT tv (NTvar  a) = dropJust (lookup a tv)
substNT tv t@(NTexist a) = t
substNT tv (NTstrict t) = NTstrict (substNT tv t)
substNT tv (NTapp t1 t2) =  NTapp (substNT tv t1) (substNT tv t2)
substNT tv (NTcons a tas) =  NTcons a (map (substNT tv) tas)
