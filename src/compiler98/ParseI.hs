module ParseI ( ParseI(..)
              , parseInterface1
              , parseInterface2
              , parseInterface3
              , parseInterface4
              , parseUntilNeed
              ) where

import Extra(pair,triple)
import Lex
import Lexical
import Syntax
--import MkSyntax(mkDeclClass)
import ParseLib
import ParseLex
import Parse2
import TokenId(tNEED,,tinterface)
import PreImp

data ParseI st tid declneed rest =
	  ParseEof  st
	| ParseNext st Bool tid  rest   -- true if visible
	| ParseNeed st declneed rest

parseAnnotVar =
   (\(_,LitInt Boxed i) -> Just i) `parseChk` lannot `ap` int `chk` rannot 

parseAnnotType =
   (\(_,LitInt Boxed i) unboxed -> (i,unboxed)) `parseChk` lannot `ap` int
                                                    `ap` optBang `chk` rannot 

parseAnnotNewType =
   lannot `revChk` optBang `chk` rannot 

optBang = True `parseChk` bang
              `orelse`
	  parse False

optSemi = () `parseChk` semi
		`orelse`
	   parse ()

parseNeedList =
     many ( ((:[]).snd) `parseAp`  (conid `orelse` varid)
	       `orelse`
            (map snd :: ([(a,b)] -> [b])) `parseChk` lit L_LCURL
                      `apCut` many  (conid `orelse` varid) `chkCut` lit L_RCURL)

parseNeedAnnot =
     Just `parseChk` optSemi `chk` lannot `chk` lit (L_ACONID tNEED)
                                      `apCut`  parseNeedList `chk` rannot
        `orelse`
     parse Nothing


parseInterface1 =
    (\(pos,modid) imports fixdecls rest -> (modid,imports,fixdecls,rest))
                `parseChk` k_interface `apCut` bigModId
                `chkCut` lit L_where `chkCut` lcurl
                `apCut` parseImpDecls
	        `apCut` parseFixDecls
		`apCut` parseRest

parseInterface2 st hideFun = triple `parseAp` parseITopDecls st [] hideFun
                                        `ap` parseNeedAnnot `ap` parseRest

parseEof = Nothing `parseChk` optSemi `chk` rcurl


parseInterface3 st needs hideFun =
  ParseEof st `parseChk` parseEof
    `orelse`
  ParseNext st `parseChk` k_interface `apCut` optBang `ap` bigModId
                                                              `apCut` parseRest
    `orelse`
  ParseNeed `parseAp` parseITopDecls st needs hideFun `apCut` parseNeedAnnot
                                                              `apCut` parseRest

parseInterface4 st hideFun =
  parseITopDecls st [] hideFun `into`
        \st -> ParseEof st `parseChk` parseEof
	           `orelse`
               ParseNext st `parseChk` k_interface  `apCut` optBang
                                               `ap` bigModId `apCut` parseRest

parseITopDecls st needs hideFuns =
     optSemi `revChk` iterateSemi0 st semi
                                   (\st -> parseITopDecl st needs hideFuns)

iterateSemi0 st s p = iterateSemi st s p
			`orelse`
		      parse st

iterateSemi st s p = p st `intoCut` (\st -> semiIterate st s p) 

semiIterate st s p = s `revChk` iterateSemi st s p
			`orelse`
		     parse st

parseITopDecl st needs hideFuns =
  cases
      [ (L_type, \pos ->
                 hType hideFuns st `parseAp` parseAnnotType
                             `ap` parseSimple `chkCut` equal `apCut` parseType)
      , (L_newtype, \pos ->
                    (hData hideFuns st . Left)
                           `parseAp` parseAnnotNewType `ap` parseContexts
                           `ap` parseSimple `apCut`
			       ( equal `revChk` someSep pipe parseConstr
				   `orelse`
			         parse [])
                           `ap` parse needs `apCut` parseDeriving)
      , (L_data, \pos ->
		 hDataPrim hideFuns st `parseChk` k_primitive 
					`apCut` conid `chk` equal
                                                      `apCut` intPrim
		   `orelse`
		 (hData hideFuns st . Right) `parseAp` unboxed
                           `ap` parseContexts `ap` parseSimple `apCut`
                               ( equal `revChk` someSep pipe parseConstr
                                    `orelse`
                                 parse [])
                           `ap` parse needs `apCut` parseDeriving)
      , (L_class, \pos ->
                  hClass hideFuns st `parseAp` parseContexts `ap` aconid
                            `ap` some avarid `apCut`
                                 (lit L_where `revChk` lcurl
                                              `revChk` parseICSigns
                                              `chk` optSemi
                                              `chkCut` rcurl
                                       `orelse`
                                  parse [])
                            `ap` (parse needs))
      , (L_instance, \pos ->
                     hInstance hideFuns st `parseAp` parseContexts
                                     `apCut` aconid `apCut` some parseInst)
      ]
      (hVarsType hideFuns st
           `parseAp` someSep comma (pair `parseAp` varid `ap` parseAnnotVar)
           `chkCut` coloncolon `apCut` parseContexts `apCut` parseType)

parseICSigns =
    id `parseChk` optSemi `ap` manySep semi parseICSign


parseICSign =
    triple `parseAp` someSep comma (pair `parseAp` varid `ap` parseAnnotVar)
           `chk` coloncolon `ap` parseContexts `ap` parseType



-- Skip until next "{-# NEED list #-}", return (Just ([],Just need,rest))
-- the same type as parseInterface3

parseUntilNeed st good bad input err =
   untilNeed input
 where
   untilNeed [] = error "Internal error in parseUntilNeed"
   untilNeed ((pos,L_EOF,_,_):input) = good (ParseEof st) input err
   untilNeed ((_,L_AVARID t,_,_):input) | t==tinterface =
       (ParseNext st `parseAp` optBang `ap` bigModId `apCut` parseRest)
       good bad input err
   untilNeed ((_,L_LANNOT,_,_):(_,L_ACONID x,_,_):input) | x == tNEED =
       ((ParseNeed st . Just) `parseAp` parseNeedList `chk` rannot
                                                      `apCut` parseRest)
       good bad input err
   untilNeed (_:input) = untilNeed input
