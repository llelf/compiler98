module IExtract
  ( countArrows, defFixity, defFixFun, fixFun, fixOne, freeType
  , iextractClass
  , iextractData, iextractDataPrim, iextractInstance, iextractType
  , iextractVarsType
  , addPreludeTupleInstances
  , needFixity, tvrPosTids, tvPosTids, tvTids
    -- re-exported from ImportState
  , getNeedIS,putModidIS
  ) where

import List
import TokenId(TokenId(..),t_Arrow,t_Tuple,ensureM,dropM,forceM,rpsPrelude
		,tEq,tOrd,tBounded,tRead,tShow,visible,tUnknown,tunknown)
import State
import IdKind
import Syntax
import Extra
import NeedLib
import AssocTree
import Memo
import SysDeps(PackedString,packString,unpackPS)
import NT
import Syntax
import OsOnly(isPrelude)
import ImportState
import Id(Id)
import Info(patchIE)

--import PrettyLib	-- debugging output only
--import PrettySyntax	-- debugging output only


-- The spike doesn't disappear if rt' is forced, instead memory usage increases!
-- ===========================
needFixity inf (ImportState visible unique orpsl rpsl needI rt st
                            insts fixity errors)  =
  case foldr (fixOne orpsl) (initAT,[]) inf of
			 -- fixity only at the beginning of interface file
    (fixAT,err) ->
	 ImportState visible unique orpsl rpsl needI rt st
                     insts (fixFun fixAT defFixFun) (err++errors)


fixFun :: AssocTree TokenId (InfixClass TokenId,Int)
          -> (TokenId -> (InfixClass TokenId,Int))
          -> (TokenId -> (InfixClass TokenId,Int))
fixFun fixAT f key =
  case lookupAT fixAT key of
    Just fix -> fix
    Nothing  -> f key

defFixFun key = defFixity
defFixity = (InfixDef,9::Int)


fixOne rps (InfixPre var,level,[fixid]) fix_err@(fix,err) =
					  -- ensureM also done in fixFun
  let fl = (InfixPre (ensureM rps var),level)
  in fixAdd fl (fixTid rps fixid) fix_err
fixOne rps (fixClass,level,ids) fixity_err =
  let fl = (fixClass,level)
  in foldr  (fixAdd fl) fixity_err (map (fixTid rps) ids)

changeType Infix = Infix
changeType InfixDef = InfixDef
changeType InfixL = InfixL
changeType InfixR = InfixR

fixTid rps (FixCon _ tid) = ensureM rps tid
fixTid rps (FixVar _ tid) = ensureM rps tid

fixAdd fl tid fix_err@(fix,err) =
 case lookupAT fix tid of
   Nothing -> (addAT fix sndOf tid fl,err)
   Just fl' ->
	if fl' == fl 
	then fix_err
	else (fix,(show tid ++ " has conflicting fixities (" ++ show fl
                   ++ " and " ++ show fl' ++ ")\n"):err)

--------------------  End duplication

---- ===========================

{- Return Id for given token of given kind. If no Id exists then 
-- create new Id -}
transTid :: Pos -> IdKind -> TokenId 
         -> a -> ImportState -> (Id,ImportState)

transTid pos kind tid _  
  importState@(ImportState visible unique orps rps needI rt st insts 
                 fixity errors) =
  let key =  (ensureM rps tid,kind)
  in  case lookupAT st key of 
        Just info -> (uniqueI info,importState)
        Nothing -> (unique, ImportState visible (unique+1) orps rps 
                              (addM needI (fst key))
			      rt
			      (addAT st combInfo key 
                                (InfoUsed unique [(kind,tid,rps,pos)]))
                              insts fixity errors)


{- Test if Id for given token of given kind exists -}
existTid :: IdKind -> TokenId -> a -> ImportState -> (Bool,ImportState)
existTid kind tid _  
  importState@(ImportState visible unique orps rps needI rt st insts 
                 fixity errors) =
  let key =  (ensureM rps tid,kind)
  in  case lookupAT st key of 
        Just info -> (True,importState)
        Nothing   -> (False,importState)
	  

--  return nothing

importData :: (TokenId->Bool) -> TokenId -> IE -> NewType -> DataKind 
              -> a -> ImportState -> ImportState

importData q tid expIn nt dk _
           importState@(ImportState visible unique orps rps needI rt st
                                    insts fixity errors)
  =
  let realtid = ensureM rps tid
      key = (realtid,TCon)
      exp = if visible then expIn else IEnone
  in case lookupAT st key of
       Just (InfoUsed u _) ->
	 let rt' = addRT visible q u tid orps TCon rt
	 in (ImportState visible unique orps rps needI rt'
                         (addAT st combInfo key (InfoData u realtid exp nt dk))
                         insts fixity errors)
       Just info@(InfoData u tid exp' nt (Data unboxed []))
         | case dk of {Data _ (_:_) -> True; _ -> False} ->
	   let rt' = addRT visible q u tid orps TCon rt
	   in ImportState visible  unique orps rps needI rt'
                          (addAT st combInfo key
                                (InfoData u tid (combIE exp exp') nt dk))
                          insts fixity errors
       Just info@(InfoData u tid exp' nt (DataNewType unboxed []))
         | case dk of {DataNewType _ (_:_) -> True; _ -> False} ->
	   let rt' = addRT visible q u tid orps TCon rt
	   in ImportState visible  unique orps rps needI rt'
                          (addAT st combInfo key
                                 (InfoData u tid (combIE exp exp') nt dk))
                          insts fixity errors
       Just info@(InfoData u' tid' exp' nt' dk') ->
	 let rt' = addRT visible q u' tid orps TCon rt
	 in seq rt' (ImportState visible unique orps rps needI rt'
                                 (addAT st combInfo key
                                        (InfoData u' tid' (combIE exp exp')
                                                  nt' dk'))
                                 insts fixity errors)
       _ -> 
	 let rt' = addRT visible q unique tid orps TCon rt
	 in (ImportState visible (unique+1) orps rps needI rt'
			 (addAT st combInfo key
                                (InfoData unique realtid exp nt dk))
                         insts fixity errors)


importClass :: (TokenId->Bool) -> TokenId -> IE -> NewType -> [Id] 
            -> a -> ImportState -> ImportState

importClass q tid expIn nt ms _
        importState@(ImportState visible unique orps rps needI rt st
                                 insts fixity errors)
  =
  let realtid = ensureM rps tid
      key = (realtid,TClass)
      exp = if visible then expIn else IEnone
  in case lookupAT st key of
       Just (InfoUsed u _) ->
	 let rt' = addRT visible q u tid orps TClass rt
	 in (ImportState visible unique orps rps needI rt'
                         (addAT st combInfo key
                                (InfoClass u realtid exp nt ms [] initAT))
                         insts fixity errors)
       Just (InfoUsedClass u _ inst) ->
	 let rt' = addRT visible q u tid orps TClass rt
	 in (ImportState visible unique orps rps needI rt'
                         (addAT st combInfo key
                                (InfoClass u realtid exp nt ms [] inst))
                         insts fixity errors)
       Just (InfoClass u tid' exp' nt' [] [] inst') ->
		 -- might be due to interface files
	 let rt' = addRT visible q u tid orps TClass rt
	 in (ImportState visible unique orps rps needI rt'
                         (addAT st combInfo key
                                (InfoClass u realtid (combIE exp exp')
                                           nt ms [] inst'))
                         insts fixity errors)
       Just info ->
	 let rt' = addRT visible q (uniqueI info) tid orps TClass rt
	 in seq rt' (ImportState visible unique orps rps needI rt' st
                                 insts fixity errors)
       _ ->
	 let rt' = addRT visible q unique tid orps TClass rt
	 in (ImportState visible (unique+1) orps rps needI rt'
			 (addAT st combInfo key
                               (InfoClass unique realtid exp nt ms [] initAT))
                         insts fixity errors)


importField :: (TokenId->Bool)
            -> [Id] -- free type variables 
            -> [(Id,Id)]  -- type context (predicates)
            -> Id  -- type constructor 
            -> Id  -- data constructor
            -> ((Maybe (a,TokenId,b),NT),Int) 
            -> c -> ImportState -> ImportState

importField q free ctxs bt c ((Nothing,_),nt) down importState = importState
importField q free ctxs bt c ((Just (p,tid,_),nt),i) down 
  importState@(ImportState visible unique orps rps needI rt st insts 
                 fixity errors) =
  let realtid = ensureM rps tid
      key = (realtid,Field)
  in case lookupAT st key of
       Just (InfoUsed u _) -> -- Selectors can never be InfoUsed
	 let rt' = addRT visible q unique tid orps Var 
                     (addRT visible q u tid orps Field rt)
	 in (ImportState visible (unique+1) orps rps needI rt' 
              (addAT -- add field name
                (addAT st combInfo (realtid,Var) -- add selector
		  (InfoVar  unique realtid IEnone (fixity realtid)
                    (NewType free [] ctxs [NTcons bt (map NTvar free),nt]) 
                    (Just 1)))
		combInfo key (InfoField u realtid IEnone [(c,i)] bt unique)) 
              insts fixity errors)
       Just (InfoField u' realtid' ie cis' bt' sel') -> 
	 let rt' =  rt
	 in  seq rt' ( -- $ here doesn't work, there is an error somwhere !!!
	   if (c,i) `elem` cis'
	   then (ImportState visible unique orps rps needI rt' st insts 
                  fixity errors)  -- unchanged, just a bit strict
	   else (ImportState visible unique orps rps needI rt' 
                  (addAT st fstOf key -- update field name 
                    (InfoField u' realtid' ie ((c,i):cis') bt' sel')) 
                    insts fixity errors))
       _ -> 
	 let rt' = addRT visible q (unique+1) tid orps Var 
                     (addRT visible q unique tid orps Field rt)
	 in (ImportState visible (unique+2) orps rps needI rt'
	      (addAT -- add field name
                (addAT st combInfo (realtid,Var) -- add selector
		  (InfoVar  (unique+1) realtid IEnone (fixity realtid)
                    (NewType free [] ctxs [NTcons bt (map NTvar free),nt]) 
                    (Just 1)))
		combInfo key (InfoField unique realtid IEnone [(c,i)]
                                        bt (unique+1)))
              insts fixity errors)


importVar :: (TokenId->Bool) -> TokenId -> IE -> NewType -> Maybe Int 
          -> a -> ImportState -> ImportState

importVar q tid exp nt annots _
          importState@(ImportState visible unique orps rps needI rt st
                                   insts fixity errors) =
  let realtid = ensureM rps tid
      key = (realtid,Var)
      fix = fixity realtid
  in case lookupAT st key of
       Just (InfoUsed u _) ->
	 let rt' = addRT visible q u tid orps Var rt
	 in addFixityNeed key fix
                         (ImportState visible unique orps rps needI rt'
		                      (addAT st combInfo key
                                             (InfoVar u realtid exp fix nt
                                                      annots))
                                      insts fixity errors)
       Just info ->
	 let rt' = addRT visible q (uniqueI info) tid orps Var rt
	 in  seq rt' (ImportState visible unique orps rps needI rt' st
                                  insts fixity errors)
       _ ->  
	 let rt' = addRT visible q unique tid orps Var rt
	 in addFixityNeed key fix
                         (ImportState visible (unique+1) orps rps needI rt'
		                      (addAT st combInfo key
                                             (InfoVar unique realtid exp
                                                      fix nt annots))
                                      insts fixity errors)


addFixityNeed key (InfixPre tid,_)
              importState@(ImportState visible unique orps rps needI rt st
                                       insts fixity errors) =
  case lookupAT rt key of  -- We use this identifier
    Just u ->
      let irealtid = ensureM rps tid
	  ikey = (irealtid,snd key)
      in
	case lookupAT rt ikey of -- so ensure that its replacement also exists,
                                 -- and force the need for it, nice if we had
                                 -- the real position but we don't.
          Just u  -> ImportState visible unique orps rps (addM needI (fst ikey))
                                 rt st insts fixity errors
          Nothing -> ImportState visible unique orps rps (addM needI (fst ikey))
                                 (addAT rt fstOf ikey (Left [noPos]))
                                 st insts fixity errors
    Nothing -> importState
addFixityNeed key inf importState = importState

--- returns unique int

importConstr q tid nt fields bt rex _
             importState@(ImportState visible unique orps rps needI rt st
                                      insts fixity errors) =
  let realtid = ensureM rps tid
      key = (realtid,Con)
  in case lookupAT st key of
       Just (InfoUsed u _) ->
	 let rt' = addRT visible q u tid orps Con rt
	 in (u,ImportState visible unique orps rps needI rt'
                           (addAT st combInfo key
                                  (InfoConstr u realtid IEnone (fixity realtid)
                                              nt fields bt))
                           insts fixity errors)
       Just info ->
	 let u = uniqueI info
	     rt' = addRT visible q u tid orps Con rt
	 in  seq rt' (u,ImportState visible unique orps rps needI rt' st
                                    insts fixity errors)
       _ -> 
	 let rt' = addRT visible q unique tid orps Con rt
	 in (unique,ImportState visible (unique+1) orps rps needI rt'
				(addAT st combInfo key
                                       (InfoConstr unique realtid rex
                                                   (fixity realtid) nt
                                                   fields bt))
                                insts fixity errors)

importMethod q tid nt rex annots bt _
             importState@(ImportState visible unique orps rps needI rt st
                                      insts fixity errors) =
  let realtid = ensureM rps tid
      key = (realtid,Method)
      fix = fixity realtid
  in case lookupAT st key of
       Just (InfoUsed u _) ->
	 let rt' = addRT visible q u tid orps Method rt
	 in (u,addFixityNeed key fix
                             (ImportState visible unique orps rps needI rt'
                                          (addAT st combInfo key
                                                 (InfoMethod u realtid IEnone
                                                             fix nt annots bt))
                                          insts fixity errors))
       Just info ->
	 let u = uniqueI info
	     rt' = addRT visible q u tid orps Method rt
	 in  seq rt' (u,ImportState visible unique orps rps needI rt' st
                                    insts fixity errors)
       _ ->
	 let rt' = addRT visible q unique tid orps Method rt
	 in (unique,addFixityNeed key fix
                                  (ImportState visible (unique+1) orps rps
                                               needI rt'
				               (addAT st combInfo key
                                                      (InfoMethod unique realtid
                                                                  rex fix nt
                                                                  annots bt))
                                               insts fixity errors))

importInstance cls con free ctxs _
               importState@(ImportState visible unique orps rps needI rt st
                                        insts fixity errors) =
  let realtid = ensureM rps cls
      key = (realtid,TClass)
  in  case lookupAT st key of
	Just info -> 
	   case addAT st fstOf key (addInstanceI con free ctxs info) of
	      st' -> seq st' (ImportState visible unique orps rps needI rt st'
                                          insts fixity errors)

storeInstance al cls con ctxs _
              importState@(ImportState visible unique orps rps needI rt st
                                       insts fixity errors) =
--strace ("storeInstance:\n  "++prettyPrintSimple 70 ppContexts ctxs
--		++"\n  "++show cls
--		++"\n  "++show con) $
  let realcls = ensureM rps cls
      realcon = ensureM rps con
      same (realcls',realcon',_,_) = realcls == realcls' && realcon == realcon'
      trans (Context pos cid [(vpos,vid)]) =
	case lookup vid al of
	  Just tvar -> Right (pos,ensureM rps cid,tvar)
	  Nothing -> Left ("Unbound type variable "++show vid
                           ++" in instance at "++strPos vpos)
  in if any same insts 
     then importState
     else
       let qctxs = map trans ctxs
       in if any isLeft qctxs
	  then ImportState visible unique orps rps needI rt st insts fixity
                           ((map dropLeft . filter isLeft ) qctxs ++ errors)
	  else ImportState visible unique orps rps needI rt st
                           ( (realcls,realcon,map snd al,map dropRight qctxs)
                             :insts)
                           fixity errors

checkInstanceCls tid down
                 importState@(ImportState visible unique orps rps needI rt st
                                          insts fixity errors) =
  case partition pred insts of
   (used,unused) -> (used,ImportState visible unique orps rps needI rt st
                                      unused fixity errors)
 where  
  realcls = ensureM rps tid
  pred (cls,con,free,ctxs) = (cls == realcls) && isJust (lookupAT st (con,TCon))


checkInstanceCon tid down
                 importState@(ImportState visible unique orps rps needI rt st
                                          insts fixity errors) =
  case partition pred insts of
   (used,unused) -> (used,ImportState visible unique orps rps needI rt st
                                      unused fixity errors)
 where  
  realcon = ensureM rps tid
  pred (cls,con,free,ctxs) = (con == realcon)
  -- if we need the type constructor, then we might need this instance
  --  && isJust (lookupAT st (cls,TClass))



-- Add (or not) an imported identifier to the renaming table.
addRT :: Bool			-- interface exports it?
      -> (TokenId->Bool)	-- must it be imported qualified?
      -> b
      -> TokenId
      -> PackedString
      -> IdKind
      -> AssocTree (TokenId,IdKind) (Either c [b])
      -> AssocTree (TokenId,IdKind) (Either c [b])

addRT False _          _ tid _   _    rt = rt
addRT True mustQualify u tid rps kind rt
  | mustQualify tid = qrt
  | otherwise       = updateAT qrt (dropM tid,kind) (combRT u)
  where
    qrt = updateAT rt (forceM rps tid,kind) (combRT u)

    combRT u (Left _) = Right [u]
    combRT u (Right us) =  Right (u:us)


---- ==================================================

iextractType :: IE -> (Int,Bool) -> (TokenId->Bool) -> a -> TokenId 
             -> [(Pos,TokenId)] -> Type TokenId 
             -> () -> ImportState -> ImportState

iextractType expInfo (depth,unboxed) q pos tid tvs typ = 
  let al = tvPosTids tvs
  in transTypes al (map snd al) [] [typ] >>>= \ nt ->
     importData q tid expInfo nt (DataTypeSynonym unboxed depth)


{-
-- extend importState by a new data type;
-- the information about the data type comes from an interface file
-}
iextractData :: IE -> (TokenId->Bool) -> Either Bool Bool -> [Context TokenId] 
             -> Int -> TokenId -> [(Pos,TokenId)] -> [Constr TokenId] 
             -> [TokenId] -> () -> ImportState -> ImportState

iextractData  expInfo q attr ctxs pos tid tvs constrs needs =
  let al = tvPosTids tvs 
      free = map snd al
  in transTypes al free ctxs (map (uncurry TypeVar) tvs
                              ++ [TypeCons pos tid (map (uncurry TypeVar) tvs)])
     >>>= \nt@(NewType free [] ctxs nts) ->
     mapS (transConstr q al free ctxs needs (last nts)) constrs >>>= \cs ->
     importData q tid -- expInfo nt 
       ((case attr of
           Left _ -> patchIE
           _      -> id) expInfo)
       nt
       (case attr of
	  Right unboxed -> Data unboxed cs
	  Left  unboxed -> DataNewType unboxed cs) >>>
     checkInstanceCon tid >>>= \ newinsts ->
     mapS0 newInstance newinsts


iextractDataPrim :: IE -> (TokenId->Bool) -> Int -> TokenId -> Int 
                 -> a -> ImportState -> ImportState

iextractDataPrim expInfo q pos tid size =
     transTid pos TCon tid >>>= \ i ->
     importData q tid expInfo (NewType [] [] [] [NTcons i []])
                              (DataPrimitive size) >>>
     checkInstanceCon tid >>>= \ newinsts ->
     mapS0 newInstance newinsts


iextractClass :: IE -> (TokenId->Bool) -> Int -> [Context TokenId] 
              -> TokenId -> TokenId 
              -> [([((a,TokenId),b)],[Context TokenId],Type TokenId)] 
              -> [TokenId] -> () -> ImportState -> ImportState

iextractClass  expInfo q pos ctxs tid tvar methods needs =
  let al = tvTids [tvar] 
  in transTypes al (map snd al) ctxs [TypeCons pos tid [TypeVar pos tvar]]
     >>>= \ nt -> 
     transContext al (Context pos tid [(pos,tvar)]) >>>= \ctx -> 
     mapS (transMethod q tvar ctx needs) methods >>>= \ms ->
     importClass q tid expInfo nt (concat ms) >>>
     checkInstanceCls tid >>>= \ newinsts ->
     mapS0 newInstance newinsts


newInstance :: (TokenId,TokenId,[Int],[(Int,TokenId,Int)]) 
            -> a -> ImportState -> ImportState

newInstance (realcls,realcon,free,ctxs) =
  mapS (\(pos,cls,tvar)-> transTid pos TClass cls >>>= \cls-> unitS (cls,tvar))
       ctxs >>>= \ ctxs ->
  transTid noPos TCon realcon >>>= \ con ->
  transTid noPos TClass realcls >>>= \ _ ->  -- Only to ensure class exists!!
  importInstance realcls con free ctxs


iextractInstance :: [Context TokenId] -> a -> TokenId -> Type TokenId 
                 -> () -> ImportState -> ImportState

-- iextractInstance ctxs pos cls typ@(TypeCons _ con _) =
iextractInstance ctxs pos cls typ =
  let con = case typ of (TypeCons _ con _) -> con; (TypeVar _ con) -> con
  in
  existTid TClass cls >>>= \qcls ->
  existTid TCon con >>>= \qcon ->
  let al = tvTids (snub (freeType typ))
  in 
    if qcls -- || qcon -- If both type class and data type exist,
                       -- then add the instance to the type class
    then
      transTypes al (map snd al) ctxs [typ]
      >>>= \(NewType free [] ctxs [NTcons c nts]) ->
      importInstance cls c free {- (map ( \ (NTvar v) -> v) nts) -} ctxs
    else
      storeInstance al cls con ctxs -- otherwise save the instance for later

-- addPreludeTupleInstances is an efficiency hack.
-- It takes a long time to parse the Prelude.hi file, and adding large
-- numbers of tuple instances to the .hi file increases compile-times
-- by 30% or more.
-- Omitting them from the .hi file and adding them by hand here, therefore
-- gives a big time saving.
addPreludeTupleInstances :: () -> ImportState -> ImportState
addPreludeTupleInstances =
  let mkCtx c v = Context noPos c [(noPos,v)]
      tuple cls n = let vars = map (visible.(:[])) (take n ['a'..]) in
                    storeInstance (tvTids vars)
					cls
					(TupleId n)
					(map (mkCtx cls) vars)
  in
    mapS0 (tuple tEq) [2..15] >>>
    mapS0 (tuple tOrd) [2..15] >>>
    mapS0 (tuple tBounded) [2..15] >>>
    mapS0 (tuple tRead) [2..15] >>>
    mapS0 (tuple tShow) [2..15]


---

iextractVarsType  expFun q postidanots ctxs typ =
   let al = tvTids (snub (freeType typ))
   in transTypes al (map snd al) ctxs [typ] >>>= \ nt ->
      mapS0 (\((pos,tid),annots) ->
               importVar q tid (expFun q tid Var) nt annots)
            postidanots

---

transMethod :: (TokenId->Bool) -> TokenId -> (Int,a) -> [TokenId]
            -> ([((b,TokenId),c)],[Context TokenId],Type TokenId) 
            -> () -> ImportState -> ([Int],ImportState)

transMethod q tvar ctx@(c,tv) needed (postidanots,ctxs,typ) =
   let al = tvTids (snub (tvar:freeType typ))
       arity = countArrows typ
   in mapS (transContext al) ctxs >>>= \ ctxs ->
      transType al typ >>>= \ typ ->
      let free = map snd al
	  nt = NewType free [] ctxs [anyNT [head free] typ]
                               -- The class context is not included in the type
      in seq arity  -- $ here doesn't work, there is an error somwhere !!!
             (mapS (\((pos,tid),annot) ->
                       let (tid',rex) = if tid `elem` needed then (tid,IEsel)
                                        else (tunknown,IEnone)
                       in importMethod q tid' nt rex (Just arity) c)
                   postidanots)

---


transConstr :: (TokenId->Bool) -> [(TokenId,Int)] -> [Int] -> [(Id,Id)] 
            -> [TokenId] -> NT -> Constr TokenId 
            -> () -> ImportState -> (Int,ImportState)

transConstr q al free ctxs needed resType@(NTcons bt _) (Constr pos cid types) =
  mapS (transFieldType al) types >>>= \ntss ->
  let all = concat ntss
      nts = map snd all
      ifs = map ((\v-> case v of Just (p,tid,i) -> Just i; _ -> Nothing) . fst)
                all
      (cid',rex) = if cid `elem` needed then (cid, IEsel)
                                        else (tUnknown bt, IEnone)
  in
  importConstr q cid' (NewType free [] ctxs (nts++[resType])) ifs bt rex
  >>>= \c->
  mapS0 (importField q free ctxs bt c) (zip all [ 1:: Int ..]) >>>
  unitS c
transConstr q al free ctxs needed resType@(NTcons bt _) 
                                  (ConstrCtx forall ectxs' pos cid types) = 
  let ce = map ( \( Context _ _ [(_,v)]) -> v) ectxs'
      e =  map snd forall 
-- filter (`notElem` (map fst al)) $ snub $ (ce ++) $ concat
--                                                $ map (freeType . snd) types
      es = zip e [1 + length al .. ]
      rex = if cid `elem` needed then IEsel else IEnone
  in
  mapS (transFieldType (es++al)) types >>>= \ntss ->
  let all = concat ntss
      nts = map snd all
      ifs = map ((\v-> case v of Just (p,tid,i) -> Just i; _ -> Nothing) . fst)
                all
      exist = map snd es
  in
  mapS (transContext (es++al)) ectxs' >>>= \ ectxs ->
  importConstr q cid (NewType (map snd al ++ exist) exist ctxs 
                              (map (\(c,v) -> NTcontext c v) ectxs
                               ++ nts ++ [resType]))
                     ifs bt rex
    >>>= \ c ->
  mapS0 (importField q free ctxs bt c) (zip all [ 1:: Int ..]) >>>
  unitS c

---

transFieldType :: [(TokenId,Int)] -> (Maybe [(Int,TokenId)],Type TokenId) 
               -> () -> ImportState 
               -> ([(Maybe (Int,TokenId,Int),NT)],ImportState)

transFieldType al (Nothing,typ) =
  transType al typ >>>= \ typ -> unitS [(Nothing,typ)]
transFieldType al (Just posidents,typ) =
  transType al typ >>>= \ typ ->
  mapS ( \ (p,v) -> transTid p Field v >>>= \ i -> 
                    unitS (Just (p,v,i),typ))  posidents


{- transform a syntactic type with context into an internal NewType -}
transTypes :: [(TokenId,Int)] -> [Id] -> [Context TokenId] -> [Type TokenId] 
              -> () -> ImportState -> (NewType,ImportState)

transTypes al free ctxs ts =
  unitS (NewType free []) =>>> 
  mapS (transContext al) ctxs 
  =>>> mapS (transType al) ts


{- transform a syntactic type variable (TokenId) into an internal type variable
-- (NT), using the given mapping
-}
transTVar :: Pos -> [(TokenId,Pos)] -> TokenId 
          -> () -> ImportState -> (NT,ImportState)

transTVar pos al v =
  unitS NTvar =>>> uniqueTVar pos al v


{- transform syntactic type variable (TokenId) into internal type variable
-- (Id), using the given mapping
-}
uniqueTVar :: Pos -> [(TokenId,Pos)] -> TokenId 
           -> () -> ImportState -> (Id,ImportState)

uniqueTVar pos al v =
  case lookup v al of
    Just v -> unitS v
    Nothing -> importError 
                 ("Unbound type variable " ++ show v ++ " at " ++ strPos pos) 
                 (0::Int)


{- transform syntactic context into internal context -}
transContext :: [(TokenId,Pos)] -> Context TokenId 
             -> () -> ImportState -> ((Id,Id),ImportState)
transContext al (Context pos cid [(vpos,vid)]) = 
  unitS pair =>>> transTid pos TClass cid =>>> uniqueTVar vpos al vid


countArrows :: Type TokenId -> Int
countArrows (TypeCons pos tid [a,f]) =
  if tid == t_Arrow 
  then 1 + countArrows f
  else 0
countArrows _ = 0::Int


{- transform a syntactic type into an internal NT type -}
transType :: [(TokenId,Int)] -> Type TokenId 
          -> () -> ImportState -> (NT,ImportState)

transType free (TypeApp  t1 t2) = 
  unitS NTapp =>>> transType free t1 =>>> transType free t2
transType free (TypeCons  pos hs types) = 
  unitS NTcons =>>> transTid pos TCon hs =>>> mapS (transType free) types
transType free (TypeVar   pos v)    = transTVar pos free v
transType free (TypeStrict pos typ) = unitS NTstrict =>>>  transType free typ

-----

{- 
-- Number the identifiers, beginning with 1.;
-- return the renaming mapping and the renamed list 
-}

tvrPosTids :: [(Pos,TokenId)] -> ([(TokenId,Id)], [(Pos, Id)])
tvrPosTids tv = (tvTids tokens, zip positions [1..])
  where
  (positions, tokens) = unzip tv


{- Number the identifiers, beginning with 1. First drop positions. -}
tvPosTids :: [(Pos,TokenId)] -> [(TokenId,Id)]
tvPosTids tv = tvTids (map snd tv)


{- Number the identifiers, beginning with 1. -}
tvTids :: [TokenId] -> [(TokenId,Id)]
tvTids tv = zip tv [1..] 

-----

{- Return a list of type variables occurring in the type. -}
freeType :: Type a -> [a]
freeType (TypeApp  t1 t2) =  freeType t1 ++ freeType t2
freeType (TypeCons  pos hs types) = concatMap freeType types
freeType (TypeVar   pos v)        = [v]
freeType (TypeStrict pos typ) = freeType typ


----- ==================================


