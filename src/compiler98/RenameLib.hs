{- ---------------------------------------------------------------------------

-}
module RenameLib(module RenameLib
	,AssocTree(..)
	,Tree ,ImportState,NT,NewType,IE,Either,Info
	,Maybe,Flags(..)) where

import List
import TokenId(TokenId(..),t_Tuple,ensureM,mkQual2,visible,mkQual3,mkQualD
              ,extractV,rpsPrelude,forceM)
import Syntax
import Scc
import NT
import Extra

import Tree234(treeMap,treeMapList)
import AssocTree
import ImportState
import IntState(checkNT)
import IExtract(tvPosTids,tvTids,freeType,fixOne,fixFun)
import State
import IdKind
import PreImp(sExp,sQual,sLG,sFix)
import PackedString(PackedString,packString,unpackPS)
import Info
import SyntaxPos
import TokenInt
import OsOnly(isPrelude)
import Flags
import Overlap		-- added in H98

data RenameState =
      RenameState
        Flags				-- flags
	Int				-- unique
	(Int,PackedString)		-- modid
	[AssocTree (TokenId,IdKind) Int] -- stack of rename (name -> unique)
	(AssocTree (TokenId,IdKind) Int) -- active rename (name	-> unique)
	(AssocTree Int Info)		-- symboltable (unique -> info)
	[(Int,[(Pos,Int)])]		-- derived   [(con,[(pos,cls)])]
	(Maybe [Int])			-- defaults
	[String]			-- errors
	[Int]				-- type Synonyms

--- The selectors for (qualFun,expFun,fixity) are defined
--- in PreImp and used here and in Fixity

{-
Destruct rename state to obtain all its elements we are interested in.
Additionally checks some properties of types.
-}
keepRS :: RenameState 
       -> (Int
          ,((TokenId,IdKind) -> Int,(TokenId,IdKind) -> Maybe Int)
          ,(Int,PackedString)
          ,Tree (Int,Info)      -- the symbol table
          ,[(Int,[(Pos,Int)])]
          ,Maybe [Int]          -- user defined defaults for Num classes
          ,[String])            -- errors

keepRS (RenameState flags unique rps rts rt st derived defaults errors 
  needCheck) =
  case checkTypes st needCheck of
    Right st ->
      (unique,getInts (lookupAll (rt:rts)),rps,st,derived,defaults,errors)
    Left x ->
      (unique,getInts (lookupAll (rt:rts)),rps,st,derived,defaults,errors ++ x)
  where
  checkTypes :: Tree (Int,Info) -> [Int] -> Either [String] (Tree (Int,Info))
  checkTypes st needCheck =
    case foldls (checkPrep st) ([],[]) needCheck of   
      -- !!! Do these checks at defining site only !!!
      (synType,newType) ->
	let keep = map fst synType
	    sccSyn = sccDepend 
                       (map ( \ (u,d) -> (u,filter (`elem` keep) d)) synType)
        in
	  case (filter isRec sccSyn) of
	    x@(_:_) -> Left (map (err2 st) x)
	    [] ->
	      case foldls ( \ (st,err) (u,c) -> 
				case isUnBoxedNT st newType [u] c of
				  Just unboxed -> 
                                    (updateAT st u (updNewType unboxed),err)  
                                    -- WORKING ON
				  Nothing -> 
                                    (updateAT st u (updNewType False)
                                    ,("Newtype " ++ 
                                      (show . tidI . dropJust . lookupAT st) u 
                                      ++ " is circular.") : err)
			  ) (st,[]) newType of
	        (st,err@(_:_)) -> Left err
	 	(st,[]) -> Right (snd (foldls fixDepth (0::Int,st) sccSyn))

  fixDepth (d,st) (NoRec u) =
    let unboxed = isUnBoxedTS st u
        d' = 1 + max d (minDepth st u) 
    in unboxed `seq` d' `seq` (d',updateAT st u (updTypeSynonym unboxed d'))

  minDepth st c = -- called for type synonyms only
    case (ntI . dropJust . lookupAT st) c of
      (NewType _ [] _ [nt]) ->
	(maximum . (0:) -- Ensures maximum works
                 . map (safeDepthI . dropJust . lookupAT st) . consNT) nt

  safeDepthI info =
    case depthI info of
      Nothing -> 0
      Just d  -> d

  isUnBoxedNT st nt ac u =
    if u `elem` ac then	-- already been here, so circular defn.
      Nothing
    else
    case lookupAT st u of
      Just info ->
	if isRealData info then	-- got the answer!
	  Just (isDataUnBoxed info)
	else
          case depthI info of
	    Just _ ->  -- type synonym, so follow the chain
	      case ntI info of
		(NewType free [] ctx [NTcons u' _]) ->
		  isUnBoxedNT st nt (u:ac) u'	      
	    Nothing -> -- newtype, so follow the chain
	      case lookup u nt of
		Just u' -> isUnBoxedNT st nt (u:ac) u'	-- defn in this module
		Nothing ->	-- defn in an imported module
                  -- error ("nhc98 needs a fix here, but I don't know how")
                  case constrsI info of
                    (coni:_) ->
                      case (ntI . dropJust . lookupAT st) coni of
                        (NewType _ _ _ [NTcons u' _,_]) ->
                          isUnBoxedNT st nt (u:ac) u'
                        _ -> error ("when renaming: newtype of imported newtype")
                    [] -> strace ("Warning: when renaming newtype of imported newtype:\n"++
                                  "  Real type of imported newtype is not visible.\n"++
                                  "  I might get boxed/unboxed info wrong.") Nothing

  isUnBoxedTS st u = -- Not circular dependency when this function is called
    case lookupAT st u of
      Nothing -> -- FAKE This is a BUG but unboxed is not used anyway
        False
      Just info ->
        case depthI info of
	  Just _ ->  -- type synonym
	    case ntI info of
	      (NewType free [] ctx [NTcons u' _]) ->
	        isUnBoxedTS st u'	      
              _ ->  -- FAKE This is a BUG but unboxed is not used anyway
                False
	  Nothing ->
	    isDataUnBoxed info

  checkPrep st (synType,newType) u =
    case lookupAT st u of
      Just info ->
        case depthI info of
	  Just _ ->  -- Only typeSyn has depth
	    case ntI info of
	      (NewType _ [] _ [nt]) ->
	        ((u,consNT nt):synType,newType)
	  Nothing -> -- If it isn't a typeSyn then it must be a newType 
            case constrsI info of
	      (coni:_) ->
		 case (ntI . dropJust . lookupAT st ) coni of
  	            (NewType _ [] _ [NTcons c _,res]) -> (synType,(u,c):newType)
                    _ -> error ("Couldn't find rhs of newtype: "++show (tidI info)++
                                "\nTwo conflicting datatype definitions?")
	      [] -> (synType,newType)		-- !!! Not a good solution !!!
      Nothing -> error ("Couldn't find definition for newtype "++show u)

  err2 ts (Rec [x]) = "Circular type synonym " ++ (show . tidI . dropJust . lookupAT ts) x ++ "."
  err2 ts (Rec (x:xs)) = "Circular dependency between the type synonyms "
		    ++ concatMap ((++", "). show . tidI . dropJust . lookupAT ts) xs ++ "and "
		    ++ (show . tidI . dropJust . lookupAT ts) x ++ "."

-- Only important that it works for data, class, type and newtype
thisModule rps (TupleId _) = rps == rpsPrelude
thisModule rps (Visible _) = False
thisModule rps (Qualified rps' _) = rps == rps'
thisModule rps (Qualified2 _ _) = False		
thisModule rps (Qualified3 _ _ _) = False

{-
Basically transform the importState into a renameState
-}
is2rs :: Flags 
      -> PackedString 
      -> (TokenId -> [TokenId]) 
      -> (Bool -> Bool -> a {- TokenId -> IdKind -> IE -}) 
      -> Overlap
      -> ImportState 
      -> Either [String] 
                (TokenId -> TokenId
                ,a
                ,RenameState
                ,Tree ((TokenId,IdKind),Either [Pos] [Int])
                )

is2rs flags mrps qualFun expFun overlap (ImportState visible unique orps rps needI irt st insts fixity errors) =
--case treeMapList undef irt of
--  [] ->
--    case foldls reorderFun (treeMap ( \ (k,Right (v:_)) -> (k,v)) irt,addAT initAT ignore unique minfo) (listAT st) of
--      (rt,ts) ->
--          Right (qualFun,expFun True True,RenameState flags (unique+1) (unique,pmrps) [] rt ts [] Nothing errors [],irt)
--  xs -> Left (map err1 xs)
  case deAlias qualFun overlap irt of
    ([],qf) ->
      case foldls reorderFun (treeMap deRight irt,addAT initAT ignore unique minfo) (listAT st) of
        (rt,ts) ->
            Right (qf,expFun True True,RenameState flags (unique+1) (unique,pmrps) [] rt ts [] Nothing errors [],irt)
    (xs,_) -> Left xs
 where
  deRight (k,Right (v:_)) = (k,v)
  deRight (k,Left _)      = (k,error ("Tripped over aliased identifier"))
  pmrps = if (isPrelude . reverse . unpackPS) mrps then rpsPrelude else mrps
  mtid = Visible pmrps
  minfo = InfoName unique mtid 0 mtid

  reorderFun (rt,at) (key,info) =
    let u = uniqueI info
        rt' = if thisModule rps (fst key) then
		addAT rt ignore key u
	      else
		rt
    in seq rt' (rt',addAT at ignore u info)

  ignore a b = b  -- Happens due to mutally recursive modules

--  undef (key,Left poss) err = Left (key,poss) : err
--  undef (key,Right [x]) err = err
--  undef (key,Right (x:xs)) err =
--	if all (x==) xs then  --- Tuples are entered twice
--	    err
--	else
--	  Right (key,x:xs) : err
--
--  err1 (Left ((tid,Method),poss)) = "The identifier " ++ show tid ++ " instantiated at " ++ mix "," (map strPos poss) ++ " does not belong to this class."
--  err1 (Left ((tid,kind),poss)) = show kind ++ ' ':show tid ++ " used at " ++ mix "," (map strPos poss) ++ " is not defined."
--  err1 (Right ((tid,kind),xs)) = show kind ++ ' ':show tid ++ " defined " ++ show (length xs) ++ " times."


--fixFixityRS ::
--    RenameState ->
--    [(InfixClass TokenId, Int, [FixId TokenId])] ->
--    (TokenId -> (InfixClass TokenId, Int), RenameState)
--
--fixFixityRS (RenameState flags unique  irps@(_,rps) rts rt st derived defaults errors needCheck) fixdecls =
-- case foldr (fixOne rps) (initAT,[]) fixdecls of
--  (fixAT,err) -> (fixFun fixAT,RenameState flags unique  irps rts rt st derived defaults (err++errors) needCheck)

-- Changed in H98 to:
fixFixityRS ::
    (TokenId -> (InfixClass TokenId, Int)) ->
    RenameState ->
    [(InfixClass TokenId, Int, [FixId TokenId])] ->
    (TokenId -> (InfixClass TokenId, Int), RenameState)

fixFixityRS oldfix rs [] = (oldfix,rs)
fixFixityRS oldfix (RenameState flags unique  irps@(_,rps) rts rt st derived defaults errors needCheck) fixdecls =
 case foldr (fixOne rps) (initAT,[]) fixdecls of
  (fixAT,err) -> (fixFun fixAT oldfix,RenameState flags unique  irps rts rt st derived defaults (err++errors) needCheck)

--------------------  End duplication

getSymbolTableRS (RenameState flags unique rps rts rt st derived defaults errors needCheck) = st
getErrorsRS (RenameState flags unique rps rts rt st derived defaults errors needCheck) =
 (RenameState flags unique rps rts rt st derived defaults [] needCheck,errors)

pushScope _ (RenameState flags unique rps rts rt st derived defaults errors needCheck) =
 RenameState flags unique rps (rt:rts) initAT st derived defaults errors needCheck
popScope _ (res,RenameState flags unique rps (rt:rts) _ st derived defaults errors needCheck) =
 (res,RenameState flags unique rps rts rt st derived defaults errors needCheck)

renameError err r fix (RenameState flags unique rps rst rt st derived defaults errors needCheck) =
 (r, RenameState flags unique rps rst rt st derived defaults (err:errors) needCheck)

--lookupAll :: [AssocTree (TokenId,IdKind) Int] -> (TokenId,IdKind) -> Maybe Int
lookupAll [] key = Nothing
lookupAll (t:ts) key =
  case lookupAT t key of
    Nothing -> lookupAll ts key
    just -> just

uniqueTid pos kind tid down renameState@(RenameState flags unique rps rts rt st derived defaults errors needCheck) =
  let key =  (sQual down tid,kind)
  in case lookupAll (rt:rts) key of 
       Just u -> (u,renameState)
       Nothing -> (unique, RenameState flags (unique+1) rps rts
					(addAT rt sndOf key unique)
					st derived defaults
					(("Unbound " ++ show kind ++ " " ++ show tid ++ " at " ++ strPos pos ++ "\n\n" ++ show (rt:rts)) : errors)
					needCheck)

fixTid kind tid down renameState@(RenameState flags unique rps rts rt st derived defaults errors needCheck) =
  let key =  (sQual down tid,kind)  --- !!! check if real name !!!
  in case lookupAll (rt:rts) key of
       Just u ->
	 case lookupAT st u of
	   Nothing ->
               (sFix down (ensureM (snd rps) (fst key)),renameState) -- hack
-- old code    ((InfixL,9::Int),renameState) -- It's an argument, and I have lost the fixity information :-(
	   Just info ->  (fixityI info,renameState)
       Nothing   ->
               (sFix down (ensureM (snd rps) (fst key)),renameState) -- hack
-- old code    ((InfixL,9::Int),renameState)
	

{-
Adds the the given identifier (position, kind of id, token with name)
to the active names in renameState.
Adds error to renameState, if identifier is already an active name,
that is, redefinition occurred.
-}
bindTid :: Pos -> IdKind -> TokenId -> a -> RenameState -> RenameState

bindTid pos kind tid _ 
  renameState@(RenameState flags unique irps@(_,rps) rts rt st derived 
               defaults errors needCheck) =
  let key =  (tid,kind)
      redefinedGlobal [rt] = lookupAT rt (forceM rps tid,kind)
      redefinedGlobal _ = Nothing
  in case lookupAT rt key of 
       Just u -> 
         if sRedefine flags 
           then renameState
	   else
	     (RenameState flags unique irps rts rt st derived defaults
		(("Redefinition of " ++ show kind ++ " " ++ show tid 
                  ++ " at " ++ strPos pos) : errors) 
                needCheck)
       Nothing   ->
	 case redefinedGlobal rts of
	   Nothing ->
	     (RenameState flags (unique+1) irps rts
			  (addAT rt sndOf key unique)
			  st derived defaults
			  errors
			  needCheck)
	   Just u -> 
             (if sRedefine flags 
                then id 
                else
		  strace ("Warning " ++ show tid 
                          ++ " is both imported and defined")) $
		 (RenameState flags unique irps rts
			      (addAT rt sndOf key u) 
                              -- ^ This catches redefinition within same scope
			      (updateAT st u clearI) derived defaults
			      errors
			      needCheck)


bindNK :: Pos -> a -> RenameState -> (TokenId,RenameState)

bindNK pos _ renameState@(RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) =
  let tid = visible (show unique)
      key = (tid,Var)
  in if sNplusK flags then
       case lookupAll (rt:rts) key of
         Nothing-> (tid, RenameState flags (unique+1) irps rts
					(addAT rt sndOf key unique)
                                        st
					-- (addAT st (\a b->b) unique (InfoUsed unique [(Var,tid,rps,pos)]))
					-- (addAT st (\a b->b) unique (InfoName unique tid 0 tid))
					-- (addAT st (\a b->b) unique (InfoVar unique tid (InfixDef,9) IEall (NewType [] [] [] [NTany 0]) (Just 0)))
                                        derived defaults
                                        errors
					needCheck)
         Just u -> (tid, RenameState flags (unique+1) irps rts
					(addAT rt sndOf key unique)
					st derived defaults
                                        (("Binding (n+k) pattern to new unique identifier at "++strPos pos): errors)
					needCheck)
     else (tid, RenameState flags (unique+1) irps rts
					(addAT rt sndOf key unique)
					st derived defaults
                                        (("(n+k) patterns are disabled - pattern at "++strPos pos): errors)
					needCheck)

checkPuns pos down renameState@(RenameState flags unique irps rts rt st derived defaults errors needCheck) =
  if sPuns flags then
    renameState
  else
    RenameState flags unique irps rts rt st derived defaults
                (("Named field puns are not Haskell'98 - used at "++strPos pos):errors)
                needCheck


{-
Checks if given identifier (kind, token) is already known as active name.
It is used to check if a field have already been included in the bindings
-}
checkTid :: a -> IdKind -> TokenId -> b -> RenameState -> (Bool,RenameState)

checkTid pos kind tid _ 
  renameState@(RenameState flags unique rps rts rt st derived defaults 
               errors needCheck) =
  let key =  (tid,kind)
  in  case lookupAT rt key of 
        Just u -> (True,renameState)
        Nothing -> (False,renameState)

---- =================

transTypes :: [(TokenId,Int)] 
           -> [Int] 
           -> [Context TokenId] 
           -> [Type TokenId] 
           -> (PackedString -> Int -> TokenId -> TokenId,TokenId -> TokenId
              ,TokenId -> IdKind -> IE,TokenId -> (InfixClass TokenId,Int)) 
           -> RenameState 
           -> (NewType,RenameState)

transTypes al free ctxs ts =
  unitS (NewType free []) =>>> 
  mapS (transContext al) ctxs =>>> 
  mapS (transType al) ts

transTVar pos al v =
  unitS NTvar =>>> uniqueTVar pos al v

uniqueTVar pos al v =
  case lookup v al of
    Just v -> unitS v
    Nothing -> renameError ("Unbound type variable " ++ show v ++ " at " ++ strPos pos) 0

transContext al (Context pos cid (vpos,vid)) = unitS pair =>>> uniqueTid pos TClass cid =>>> uniqueTVar vpos al vid


transType :: [(TokenId,Int)] 
          -> Type TokenId
          -> (PackedString -> Int -> TokenId -> TokenId,TokenId -> TokenId
             ,TokenId -> IdKind -> IE,TokenId -> (InfixClass TokenId,Int)) 
          -> RenameState 
          -> (NT,RenameState)

transType al (TypeApp  t1 t2) = unitS NTapp =>>> transType al t1 =>>> transType al t2
transType al (TypeCons  pos hs types) = unitS NTcons =>>> uniqueTid pos TCon hs =>>> mapS (transType al) types
transType al (TypeVar   pos v)       = transTVar pos al v
transType al (TypeStrict pos t)       = unitS NTstrict =>>> transType al t

----- ==================================


defineDefault :: [Type Int] -> a -> RenameState -> RenameState

defineDefault types down  
  (RenameState flags unique rps rts rt st derived Nothing errors needCheck) =
    case partition ( \ nt -> case nt of TypeCons _ _ [] -> True; _ -> False) 
           types of
      (cs,[]) -> 
        RenameState flags unique rps rts rt st derived 
          (Just (map ( \ (TypeCons _ con _) -> con) types)) errors needCheck
      (_,es) ->  
        RenameState flags unique rps rts rt st derived Nothing
	  (("Illegal type in default at " ++ strPos (getPos es)):errors) 
          needCheck     
defineDefault types down  
  (RenameState flags unique rps rts rt st derived defaults errors needCheck) =
    RenameState flags unique rps rts rt st derived defaults 
      (("Redefinition of defaults at " ++ strPos (getPos types)) :errors) 
      needCheck

defineType :: TokenId 
           -> NewType 
           -> (a,b,TokenId -> IdKind -> IE,c) 
           -> RenameState 
           -> RenameState

defineType tid nt down 
  (RenameState flags unique irps@(_,rps) rts rt st derived defaults 
  errors needCheck) =
  let realtid = ensureM rps tid
      key = (tid,TSyn)
  in case lookupAT rt key of
       Just u -> RenameState flags unique irps rts rt
			 (addAT st combInfo u {-(realtid,TSyn)-} (InfoData u realtid (sExp down tid TSyn) nt (DataTypeSynonym False 0)))
			 derived defaults errors (u:needCheck)


defineClass :: Int 
            -> TokenId 
            -> NewType 
            -> [(Int,Int)] 
            -> (a,b,TokenId -> IdKind -> IE,c) 
            -> RenameState 
            -> RenameState

defineClass pos tid nt mds down 
  (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors 
     needCheck) =
  let realtid = ensureM rps tid
      key = (tid,TClass)
      (ms,ds) = unzip mds
  in case lookupAT rt key of
       Just u ->
        let newst = addAT st combInfo u {-(realtid,TClass)-} 
                     (InfoClass u realtid (sExp down tid TSyn) nt ms ds initAT)
        in case checkNT pos (strAT st) nt of
          Nothing -> 
            RenameState flags unique irps rts rt newst derived defaults 
              errors needCheck
          Just err -> 
             RenameState flags unique irps rts rt newst derived defaults 
               (err:errors) needCheck


defineDataPrim tid nt size down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck)  =
  let realtid = ensureM rps tid
      key = (tid,TCon)
  in case lookupAT rt key of
       Just u -> (u,RenameState flags unique irps rts rt 
			(addAT st combInfo u {-(realtid,TCon)-} (InfoData u realtid (sExp down tid TCon) nt (DataPrimitive size)))
			 derived defaults errors needCheck
                 )

defineData d tid nt cs   down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck)  =
  let realtid = ensureM rps tid
      key = (tid,TCon)
  in case lookupAT rt key of
       Just u ->
         let (needCheck',dk) =
		case d of
		  Just unboxed -> (needCheck,Data unboxed cs)
		  Nothing -> (u:needCheck,DataNewType False cs) -- unboxed fixed by keepRS
	 in (u,RenameState flags unique irps rts rt 
			(addAT st combInfo u {-(realtid,TCon)-} (InfoData u realtid (sExp down tid TCon) nt dk))
			 derived defaults errors needCheck')

defineMethod  pos tid nt arity c down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) =
  let realtid = ensureM rps tid
      key = (tid,Method)
  in case lookupAT rt key of
       Just u ->
         let newst = addAT st combInfo u {-(realtid,Method)-} (InfoMethod  u realtid (sFix down realtid) nt (Just arity) c)
         in case checkMNT c nt of
              Nothing ->
                (u,RenameState flags unique irps rts rt newst derived defaults errors needCheck)
              Just err ->
                (u,RenameState flags unique irps rts rt newst derived defaults (err:errors) needCheck)
 where
  checkMNT c nt@(NewType free@(cv:_) [] ctxs nts) =
    case filter ((cv==) . snd) ctxs of
      [] -> checkNT pos (strAT st) nt
      [x] -> Just ("Illegal restriction " ++ strAT st (fst x) ++ " for type variable in type signature at " ++ strPos pos)
      xs -> Just ("Illegal restriction " ++ mixCommaAnd (map (strAT st . fst) xs) ++ " for type variable in type signature at " ++ strPos pos)

defineConstr  tid nt fields bt  down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) = 
  let realtid = ensureM rps tid
      key = (tid,Con)
  in case lookupAT rt key of
       Just u -> (u,RenameState flags unique irps rts rt
			 (addAT st combInfo u {-(realtid,Con)-} (InfoConstr  u realtid  (sFix down realtid) nt fields bt))
			 derived defaults errors needCheck)

defineField typtid bt c ((Nothing,_),_) down up = (Nothing,up)
defineField typtid bt c ((Just (p,tid,_),_),i) down up@(RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) = 
  let realtid = ensureM rps tid
      key = (tid,Field)
  in
    case lookupAT rt (tid,Field) of
      Just u ->
        case lookupAT st u of
	  Just (InfoField u' realtid' cis' bt' iSel') ->
	    if bt == bt'
	    then (Nothing,RenameState flags unique irps rts rt
			 (addAT st fstOf u' {-(realtid,Field)-} (InfoField u' realtid' ((c,i):cis') bt' iSel'))
			 derived defaults errors needCheck)
	    else (Nothing,RenameState flags unique irps rts rt st
			 derived defaults (("Field " ++ show tid ++ " at " ++ strPos p ++ " is already defined"):errors) needCheck)
	  Just u -> (Nothing,up)
	  Nothing ->
	    case lookupAT rt (tid,Var) of
	      Just selu ->
		case lookupAT rt key of
		  Just u ->
		      (Just (p,u,selu)
		      ,RenameState flags unique irps rts rt
			  	   (addAT (addAT st combInfo u {-(realtid,Field)-} (InfoField u realtid [(c,i)] bt selu))
				    combInfo selu (InfoVar selu realtid (sFix down realtid) (case sExp down typtid TCon of
												IEall -> IEsel
												e -> IEnone) NoType (Just 1)))
				   derived defaults errors needCheck)

localTid rps u tid = mkQual3 (Visible rps) (t_Tuple u) tid
globalTid rps u tid = ensureM rps tid

defineVar tid  down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) = 
  let key = (tid,Var)
  in case lookupAT rt key of
       Just u ->
	 let realtid = sLG down rps u tid
	 in  (u,RenameState flags unique irps rts rt
			(addAT st combInfo u {-(realtid,Var)-}
				 (InfoVar u realtid  (sFix down (ensureM rps tid)) (sExp down tid Var) NoType Nothing))
				 derived defaults errors needCheck)


defineDefaultMethod tid  down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) = 
  let realtid = mkQualD rps tid
      skey = (tid,Method)
  in case lookupAT rt skey of
	Nothing -> error ("***defineDefaultMethod(1) " ++ show skey ++ "\n" ++ show rt)
        Just u ->
         case lookupAT st u of 
	   Nothing -> error ("***defineDefaultMethod(1) " ++ show skey ++ " " ++ show u ++ "\n" ++ show rt)
           Just (InfoMethod _ _ fix nt annot iClass) ->
	     (unique,RenameState flags (unique+1) irps rts rt
			 (addAT st combInfo  unique {-(realtid,MethodDefault)-} (InfoDMethod unique realtid nt annot iClass))
			 derived defaults errors needCheck)

defineInstMethod tid  down (RenameState flags unique irps@(_,rps) rts rt st derived defaults errors needCheck) = 
  let realtid = mkQual2 (t_Tuple unique) (ensureM rps tid)
      key = (tid,MethodInstance)
  in (unique,RenameState flags (unique+1) irps rts rt
		 (addAT st combInfo  unique {-(realtid,MethodInstance)-} (InfoIMethod unique realtid NoType Nothing (0::Int)))
		 derived defaults errors needCheck)

defineDerived con posis down (RenameState flags unique rps rts rt st derived defaults errors needCheck) = 
  RenameState flags unique rps rts rt st ((con,posis):derived) defaults errors needCheck

strAT st i = (show . tidI . dropJust . lookupAT st) i

