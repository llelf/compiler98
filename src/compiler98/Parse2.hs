module Parse2(parseConstr, parseContexts, parseDeriving, parseFixDecls
             ,parseFixDecl,parseImpDecls, parseInst, parseSimple, parseType
	     ,parseExports,parseStrict) where

import Extra(pair,triple,noPos,Pos(..))
import Lex
import Syntax
import MkSyntax
import ParseLib
import ParseLex
import SyntaxPos
import TokenId(t_Arrow,t_Tuple)

parseExports =
    id `parseChk` lpar `apCut` someSep comma parseExport `chk` rpar
        `orelse`
    parse []

parseExport =
    (uncurry ExportModid) `parseChk` lit L_module `apCut` aconid	-- 1.3
        `orelse`
    (uncurry ExportModid) `parseAp` aconid `chk` dotdot			-- 1.2
        `orelse`
    (\e -> ExportEntity (getPos e) e) `parseAp` parseEntity

parseImpDecls =
    manysSep semi parseImpDecl

parseImpDecl =
    Importas `parseChk` lit L_import `ap` aconid `chk` k_as `ap` aconid `ap` parseImpSpec -- added in H98
	`orelse`
    Import `parseChk` lit L_import `ap` aconid `ap` parseImpSpec
	`orelse`
    ImportQas `parseChk` lit L_import `chk` lit L_qualified `ap` aconid `chk` k_as `ap` aconid `ap` parseImpSpec -- No longer FAKE
	`orelse`
    ImportQ `parseChk` lit L_import `chk` lit L_qualified `ap` aconid `ap` parseImpSpec -- FAKE


parseImpSpec =
    (NoHiding []) `parseChk` k_unit -- fix for import Module()
        `orelse`
    NoHiding `parseChk` lpar `apCut` manySep comma parseEntity `chk` rpar
        `orelse`
    Hiding `parseChk` lit L_hiding `chk` lpar `apCut` manySep comma parseEntity `chk` rpar         
        `orelse`
    parse (Hiding [])

parseEntity =
    (uncurry EntityTyConCls) `parseAp` aconid `chk` lpar `chk` dotdot `chk` rpar
	`orelse`
    ( \ (pos,x) -> EntityTyCon pos x []) `parseAp` aconid `chk` tuple0
        `orelse`
    (uncurry EntityTyCon) `parseAp` aconid `chk` lpar `ap` (manySep comma conid) `chk` rpar
	`orelse`
    (uncurry EntityTyCls) `parseAp` aconid `chk` lpar `ap` (manySep comma varid) `chk` rpar
        `orelse`
    (uncurry EntityTyCon) `parseAp` aconid `ap` (parse []) 
	`orelse`
    (uncurry EntityVar) `parseAp` varid


parseFixDecls =
    semi `revChk` parseFixDecls
        `orelse`
    manysSep semi parseFixDecl

defint d =
    intPrim
        `orelse`
    parse d

parseInfix = InfixL `parseChk` lit L_infixl
               `orelse`
             InfixR `parseChk` lit L_infixr
               `orelse`
             Infix  `parseChk` lit L_infix

parseFixDecl =
        triple `parseAp` parseInfix `ap` defint 9 `ap` someSep comma parseFixId
               `orelse`
        k_prefix `into` \ _ -> varid `into` \ (p,v) -> defint 9 `into` \ l -> parseFixId `into` \ fid ->
	parse (InfixPre v,l,[fid])

parseFixId = 
        (uncurry FixCon) `parseAp` conop
	    `orelse`
        (uncurry FixVar) `parseAp` varop


parseType = 
    parseBType  `into` parseCType
		`into` \ t1 -> (
		    (\ pos t2 -> TypeCons pos t_Arrow [t1,t2]) `parseAp` rarrow `apCut` parseType
			`orelse`
		    parse t1)

parseCType t1 = 
    parseAType `into` (\ t2 -> parseCType (TypeApp t1 t2))
	`orelse`
    parse t1

parseBType  = 
    ( \ (pos,c) ts -> TypeCons pos c ts) `parseAp` conid `ap` some parseAType
        `orelse`
    parseAType

parseAType = 
    (uncurry TypeVar) `parseAp` varid
        `orelse`
    (\(pos,c) -> TypeCons pos c []) `parseAp` conid
        `orelse`
    mkParType `parseAp` lpar `apCut` manySep comma parseType `chk` rpar
        `orelse`
    mkTypeList `parseAp` lbrack `apCut` parseType `chk` rbrack
    

parseContexts =
    lpar `revChk` manySep comma parseContext `chk` rpar `chk` impl
        `orelse`
    (:[]) `parseAp` parseContext `chk` impl
        `orelse`
    parse []

parseContext = 
    (\ (pos,c) pt_t -> Context pos c pt_t)  `parseAp` conid `ap` varid

parseSimple =
    (uncurry Simple) `parseAp` conid `ap` many varid

parseConstr =
	(k_forall `revChk` some varid `into` \ free -> k_dot `revChk` parseConstr' free)
	  `orelse`
	parseConstr' []

parseConstr' free =
    (\a (pos,op) b -> (if null free 
		       then Constr pos op [a,b]
		       else ConstrCtx free [] pos op [a,b]))
			 `parseAp` parseOneFieldType `ap` conop `ap` parseOneFieldType
        `orelse`
    (\ctxs (pos,op) a ->
	case ctxs of
	   [] ->  (if null free 
		   then Constr            pos op (concat a)
		   else ConstrCtx free [] pos op (concat a))
           _  ->  ConstrCtx free ctxs pos op (concat a)) `parseAp` parseContexts 
 				                         `ap` conid `ap` many parseManyFieldType

parseOneFieldType =
     (\ field typ -> (Just [field],typ)) `parseChk` lcurl `apCut` varid `chk` coloncolon `ap` parseSBType `chk` rcurl 
	`orelse`
     (pair Nothing) `parseAp` parseSBType


parseManyFieldType =
     lcurl `into` (\ _ -> manySep comma parseManyFieldType' `chk` rcurl)  -- { v1,...,v2::typeN , ...  w1,...,wN::typeN } 
	`orelse`
     ((:[]).pair Nothing) `parseAp` (parseStrict parseAType)

parseManyFieldType' =
     (\ fields typ -> (Just fields,typ)) `parseAp` someSep comma varid `chk` coloncolon `ap`  parseSBType  -- v1,...,v2::typeN 

parseSBType =
    parseStrict parseType
	`orelse`
    parseType

parseStrict p =
  TypeStrict `parseAp` bang `ap` p
	`orelse`
  p

parseDeriving =
    lit L_deriving `revChk` lpar `revChk` manySep comma conid `chk` rpar
        `orelse`
    (:[]) `parseChk` lit L_deriving `apCut` conid
        `orelse`
    parse []

parseInst =
    (\ (p,c) -> TypeCons p c []) `parseAp` conid	-- type without arguments
        `orelse`
    lpar `revChk` parseInst' `chkCut` rpar		-- type inside paranthesis
        `orelse`
    (\p (_,pat) -> mkInstList p pat) `parseAp` lbrack `apCut` varid `chk` rbrack -- the list type

parseInst' =
    lpar `revChk` parseInst' `chkCut` rpar		-- useless extra paranthesis
	`orelse`
    varid `revAp` ((\pos (pa,a) (pb,b) -> TypeCons pos t_Arrow [TypeVar pb b,TypeVar pa a]) `parseAp` rarrow `apCut` varid
                        `orelse`
                   (\a b@(p,_) -> mkParInst p (b:a)) `parseChk` comma `apCut` someSep comma varid
                  )
        `orelse`
    mkAppInst `parseAp` conid `ap` many varid
        `orelse`
--    (TypeCons noPos (t_Tuple 0) []) `parseChk` lpar `chk` rpar
--        `orelse`
    parse (TypeCons noPos (t_Tuple 0) [])
