module PreImport (qualRename, preImport) where

import List(partition)
import MergeSort
import TokenId(TokenId(..),tPrelude,t_Arrow,ensureM,forceM,dropM,
               rpsPrelude,rpsBinary,t_List)
import PackedString(PackedString,packString,unpackPS)
import Syntax
import Kind
import AssocTree
import Memo
import Tree234(treeMapList)
import Extra
import Lexical(Lex,PosToken(..),PosTokenPre(..),LexState(..),lexical)
import ParseCore(Parser(..),ParseBad(..),ParseError(..)
                ,ParseGood(..),ParseResult(..),parseit)
import ParseI
import Flags
import OsOnly
import Scc
import IExtract
import Info

qualRename impdecls = qualRename' qTree 
 where
  qualRename' t q@(Qualified t1 t2) =
    case lookupAT t t1 of
	Nothing -> [q]
	Just ts -> map (\t'-> Qualified t' t2) ts
  qualRename' t v = [v]

  qTree = foldr qualR initAT impdecls

  qualR (Import _ _)                      t = t
  qualR (ImportQ   _ _)                   t = t
  qualR (ImportQas (pos,Visible tid) (pos',Visible tid') _) t = addAT t (++) tid' [tid]
  qualR (Importas  (pos,Visible tid) (pos',Visible tid') _) t = addAT t (++) tid' [tid]


---- ===================================
-- shorten rpsl = if (isPrelude . reverse . unpackPS) rpsl then rpsPrelude else rpsl


preImport flags mtid@(Visible mrps) need expdecls impdecls =
  case transImport impdecls of
    Left err -> Left err
    Right impdecls ->
      if null expdecls || (isJust . lookupAT expAT) (mtid,Modid)
      then Right (expFun1, map (mkNeed need expAT) impdecls)
      else Right (expFun2 mrps expAT, map (mkNeed need expAT) impdecls)
 where
  expAT = mkExportAT expdecls


-- transImport order the import files (with prelude last), insert
-- qualified import of prelude and check that all imports are consistent

				--        errors   module notqual qual              explicit     hiding
transImport :: [ImpDecl TokenId] -> Either String [(TokenId,Bool,  Bool,  Either [(TokenId,IE)] [(TokenId,IE)])]
transImport impdecls =
  case concatMap checkImport impdecls2 of
    err@(_:_) -> Left (unlines err)
--  [] -> case checkForMultipleImport impdecls2 of	-- removed in H98
--          err@(_:_) -> Left (unlines err)		-- removed in H98
--          [] ->  Right (map finalTouch impdecls2)	-- removed in H98
    [] ->  Right (map finalTouch impdecls2)

 where

  impdecls2 =  (sortImport . traverse initAT False)
                     (ImportQ (noPos,tPrelude) (Hiding []) :
                      --ImportQ (noPos,Visible rpsBinary) (Hiding []) :
                        --ImportQ (noPos,vis "PrelRatio") (NoHiding [EntityTyConCls noPos (vis "Rational"), EntityVar noPos (vis "%")]) :
                            impdecls)
  vis = Visible . packString . reverse

  sortImport impdecls = ( map snd 
			. mergeSortCmp (error "Fail in PreImport.transImport\n") cmpFst 
			. map ( \ (k,v) -> if k == tPrelude then (Right k,(k,v)) else (Left k,(k,v)) )
			) impdecls

  traverse :: AssocTree TokenId (Bool,Maybe [TokenId],[(Pos,Either [(TokenId,IE)] [(TokenId,IE)])])
                -> Bool
		-> [ImpDecl TokenId]
		-> [(TokenId,(Bool,Maybe [TokenId],[(Pos,Either [(TokenId,IE)] [(TokenId,IE)])]))]

  traverse acc True  []      = treeMapList (:) acc
  traverse acc False []      = traverse acc False [Import (noPos,tPrelude) (Hiding [])]
  traverse acc prel  (x:xs)  =
    case extractImp prel x of
      (prel',tid,info) ->
        traverse (addAT acc comb tid info) prel' xs


  comb (nq,Nothing,xs) (nq',q',     xs') = (nq || nq', q'           ,xs++xs') -- Not qualified , import specification
  comb (nq,q,      xs) (nq',Nothing,xs') = (nq || nq', q            ,xs++xs') -- Not qualified , import specification
  comb (nq,Just q, xs) (nq',Just q',xs') = (nq || nq',Just (q ++ q'),xs++xs') -- Not qualified , import specification

  extractImp prel (ImportQ  (pos,tid) impspec)  = (prel,tid,(False,Just [], [(pos,extractSpec impspec)]))
  extractImp prel (ImportQas (pos,tid) (apos,atid) impspec) =
                                                  (prel,tid,(False,Just [atid],[(pos,extractSpec impspec)]))
  extractImp prel (Import (pos,tid) impspec)    = (prel || tid == tPrelude
						      ,tid,(True, Nothing, [(pos,extractSpec impspec)]))
  extractImp prel (Importas (pos,tid) (apos,atid) impspec) =
                                                  (prel,tid,(True,Just [atid],[(pos,extractSpec impspec)]))

  extractSpec (NoHiding entities) = Left (map extractImpEntity entities)
  extractSpec (Hiding entities) = Right (map extractImpEntity entities)

  extractImpEntity e = case funFix (extractEntity e) of ((tid,kind),ie) -> (tid,ie)

  checkImport :: (TokenId,(Bool,Maybe [TokenId],[(Pos,Either [(TokenId,IE)] [(TokenId,IE)])])) -> [String]
  checkImport (tid,(nq,q,pos_spec)) =
    case partition (isLeft . snd) pos_spec of
      ([],hide)  -> []  -- Only explicit hide
      (imp,[])   -> []  -- Only explicit imports
      (imp,hide) ->
	if (null . filter (not.null) . map (dropRight . snd)) hide
	then []         -- Ok as all hidings are empty
	else ["Conflicting imports for " ++ show tid ++", used both explicit imports (at" ++ (mixCommaAnd . map (strPos . fst)) imp 
		    ++ ") and explicit hidings (at " ++  (mixCommaAnd . map (strPos . fst)) hide ++")."]


  finalTouch :: (TokenId,(Bool,Maybe [TokenId],[(Pos,Either [(TokenId,IE)] [(TokenId,IE)])]))
		 -> (TokenId,Bool,Bool,Either [(TokenId,IE)] [(TokenId,IE)])
  finalTouch (tid,(nq,q,pos_spec)) = -- import specification is ok if finalTouch is called
    case partition (isLeft . snd) pos_spec of
      (imp,[])   ->  -- Only explicit import
	(tid,nq,isJust q,Left (concatMap (dropLeft . snd) imp))
      (_,hide) -> -- Either only explicit hide, or explicit import and empty explict hide.
	(tid,nq,isJust q,Right (concatMap (dropRight . snd) hide))


checkForMultipleImport imports = 
    case foldr prepare (initAT,[]) imports of
      (qm,qas) ->
	case (filter (elemM qm) qas,filter ((1/=) . length) (group qas)) of
	  (qas,qas2) ->
	    map ( \ tid -> "Can not rename a module to " ++ show tid ++ " as another module with that name is imported qualified.") qas ++
	    map ( \ tids -> "More than one module is renamed to " ++ show (head tids) ++ ".") qas2
 where
  prepare (tid,(nq,Just tids,pos_spec)) (qm,qas) = (addM qm tid,tids++qas)
  prepare _ (qm,qas) = (qm,qas)



------------------------------------------------------------------------------


expFun1 v q tid kind = IEall

expFun2 rps expAT v q tid kind =
{-
  case lookupAT expAT (tid,kind) of
    Just imp -> imp
    Nothing  -> 
-}
      case lookupAT expAT (dropM tid,kind) of
        Just imp | v -> imp
        _  -> 
          case lookupAT expAT (forceM rps tid,kind) of
            Just imp | q -> imp
            _  -> IEnone


mkExportAT expdecls =
   expAT
 where
  expAT :: AssocTree (TokenId,Kind) IE
  expAT = foldr exp initAT (map preX expdecls)

  exp (key,value) t = addAT t combIE key value

  preX (ExportEntity _ e) = funFix (extractEntity e)
  preX (ExportModid _ tid) = ((tid,Modid),IEall)



------

funFix ((tid,k),e) | k == TCon && (tid == t_Arrow || tid == t_List) = ((dropM tid,k),e)  -- must use == TCon as we also want to match TC
funFix x = x

extractEntity (EntityVar  pos tid) = ((tid,Var),IEall)
extractEntity (EntityTyConCls pos tid) = ((tid,TC),IEall)
extractEntity (EntityTyCon  pos tid []) = ((tid,TCon),IEabs)
extractEntity (EntityTyCon  pos tid ids) = ((tid,TCon),IEall)  -- Don't care about checking that all constructors are correct
extractEntity (EntityTyCls  pos tid ids) = ((tid,TClass),IEall)  -- Don't care about checking that all methods are correct

--------------------------------------
  

--- The selectors for (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) are defined
--- in PreImp and used in ParseI

mkNeed needM expAT (vt@(Visible rps),nq,q,Left ei) =  -- explicit import
   (rps
   ,\needI -> any (needFun needI)
   ,(hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType))
 where
  impT = foldr ( \ (k,v) t -> addAT t combIE k v ) initAT ei

  expFun = 
	case lookupAT expAT (vt,Modid) of
          Just _ -> expFun1
          Nothing -> expFun2 rps expAT

  needFun (orps,rps,needI) ns@(n:_) =
        isJust (lookupAT needI (ensureM rps n))        -- is used by other interface (real name)
     || (q && any (isJust . lookupAT needM . forceM orps) ns) -- qualified imported and used qualified
     || let n' = dropM n
        in case lookupAT impT n' of   	      -- imported and used
		Just IEabs -> 	   isJust (lookupAT needM n')
		Just IEall -> any (isJust .lookupAT needM . dropM) ns
		Nothing -> False

  hideDeclType st attr (Simple pos tid tvs) typ = 
    case lookupAT impT (dropM tid) of
      Just _  -> iextractType (expFun nq q tid TSyn) attr nq    q pos tid tvs typ () st
      Nothing -> iextractType IEnone            attr False q pos tid tvs typ () st

  hideDeclData st attr ctxs (Simple pos tid tvs) constrs der =
    case lookupAT impT (dropM tid) of
      Just IEall -> iextractData  (expFun nq q tid TCon) nq    q attr ctxs pos tid tvs constrs () st
      Just IEabs -> iextractData  (expFun nq q tid TCon) nq    q attr ctxs pos tid tvs (if q then constrs else []) () st
      Nothing ->    iextractData  IEnone            False q attr ctxs pos tid tvs (if q then constrs else []) () st

  hideDeclDataPrim st (pos,tid) size =
    case lookupAT impT (dropM tid) of
      Just _  -> iextractDataPrim (expFun nq q tid TCon) nq    q pos tid size () st
      Nothing -> iextractDataPrim IEnone            False q pos tid size () st

  hideDeclClass st  ctxs (pos,tid) tvar methods =
    case lookupAT impT (dropM tid) of
      Just IEall ->  iextractClass  (expFun nq q tid TClass) nq    q pos ctxs tid (snd tvar) methods () st
      Just IEabs ->  iextractClass  (expFun nq q tid TClass) nq    q pos ctxs tid (snd tvar) (if q then methods else []) () st
      Nothing -> iextractClass  IEnone              False q pos ctxs tid (snd tvar) (if q then methods else []) () st

  hideDeclInstance st ctxs (pos,cls) typ =
    iextractInstance ctxs pos cls typ () st

  hideDeclVarsType st postidanots ctxs typ =    -- interface files should never depend on functions
{- we don't create interface files with more than one function/type
    case filter (isJust . lookupAT impT . dropM . snd . fst) postidanots of
      [] -> st
      postidanots ->
-}
	 iextractVarsType  expFun nq q postidanots ctxs typ () st


mkNeed needM expAT (vt@(Visible rps),nq,q,Right eh) = -- explicit hiding
   (rps
   ,\needI -> any (needFun needI)
   ,(hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType))
 where
  hideT = foldr (flip addM) initM (map fst eh)

  (needFun,expFun) =
	case lookupAT expAT (vt,Modid) of
          Just _ -> (needFun1, expFun1)
          Nothing -> (needFun2, expFun2 rps expAT)

  needFun1 (orps,rps,needI) (n:ns) = 
       isNothing (lookupM hideT (dropM n))       -- not hidden (all identifiers used because M.. in export)

  needFun2 (orps,rps,needI) ns@(n:_) =
         any (isJust . lookupAT needI . ensureM rps) ns       -- is used by other interface (real name)
     || (q && any (isJust . lookupAT needM . forceM orps) ns)  -- qualified import and used qualified
     || ((isNothing . lookupM hideT . dropM) n && 	      -- not hidden and is used
		any (isJust . lookupAT needM . dropM) ns)

  needMethods ns = -- isn't correct if M.. in export list, but won't be used in that case unless class is explicit hidden
        (q && any (isJust . lookupAT needM . forceM rps) ns)  -- qualified import and used qualified (No methods if in interface part)
     || any (isJust . lookupAT needM . dropM) ns

  hideDeclType st attr (Simple pos tid tvs) typ = 
    case lookupM hideT (dropM tid) of
      Just _ ->  iextractType IEnone            attr False q pos tid tvs typ () st -- used  in interface file
      Nothing -> iextractType (expFun nq q tid TSyn) attr nq    q pos tid tvs typ () st

  hideDeclData st attr ctxs (Simple pos tid tvs) constrs der =
    case lookupM hideT (dropM tid) of
      Just _ ->  iextractData  IEnone            False q attr ctxs pos tid tvs (if q then constrs else []) () st
      Nothing -> iextractData  (expFun nq q tid TCon) nq    q attr ctxs pos tid tvs constrs () st

  hideDeclDataPrim st (pos,tid) size =
    case lookupM hideT (dropM tid) of
      Just _  -> iextractDataPrim IEnone            False q pos tid size () st -- used by import
      Nothing -> iextractDataPrim (expFun nq q tid TCon) nq    q pos tid size () st

  hideDeclClass st ctxs (pos,tid) tvar methods =
    case lookupM hideT (dropM tid) of
      Just _  -> iextractClass IEnone False q pos ctxs tid (snd tvar) (if q then methods else []) () st
      Nothing -> 
        case expFun nq q tid TClass of
	  IEnone | not q && (not . needMethods . map (snd . fst) . concat . map fst3) methods ->
	      iextractClass IEnone nq    q pos ctxs tid (snd tvar) []      () st
	  exp ->
	      iextractClass exp    nq    q pos ctxs tid (snd tvar) methods () st

  hideDeclInstance st ctxs (pos,cls) typ =
    iextractInstance ctxs pos cls typ () st

  hideDeclVarsType st postidanots ctxs typ =   -- interface files should never depend on functions
{-  We don't create interface files with more than one function/type!
    case filter ( (\ tid -> (isNothing . lookupM hideT . dropM) tid && (isNothing . lookupM hideT) tid) . snd . fst) postidanots of
      [] ->  st
      postidanots -> 
-}
	iextractVarsType  expFun nq q postidanots ctxs typ () st


