module Main where

import IO
import System

import Scc
import Error

import Tree234
import AssocTree
import Syntax
import PosCode

import OsOnly
import Extra(Pos(..),mix,mixSpace,jRight,jLeft,noPos,strPos,showErr,mixLine,pair,fst3,thd3)
import State(State0(..))
import ImportState
import IntState(IntState,dummyIntState,getSymbolTable,getErrors,strIS,mrpsIS)
import PPLib
import NeedLib(initNeed)
import RenameLib(getSymbolTableRS,RenameState,getErrorsRS)
import PreImport
import ParseCore(Parser(..),ParseBad(..),ParseError(..),ParseGood(..),ParseResult(..)
                ,parseit)

import Flags
import StrSyntax(strType,StrId(..))
import SyntaxPos	-- DW
import PPSyntax(ppModule,ppDecl,ppDecls,ppImpDecls,ppInterface,ppFun,ppClassCodes)
import StrPos(strPCode)

import TokenId(TokenId(..),t_Arrow,t_List,tPrelude,tminus,tnegate,tTrue)
import Kind(Kind(..))
import Lex(Lex,LexAnnot)  -- need show

import Unlit(unlit)
import Lexical(PosToken(..),PosTokenPre(..),LexState(..),lexical)
import Parse(parseProg)
import Need
import Import(importOne)
import IExtract(getNeedIS)
import Rename(rename)
import DbgDataTrans
import DbgTrans
import DbgDumpSRIDTable
import DbgDumpSRIDTableC
import EmitState
import Derive(derive)
import Extract(extract)
import Remove1_3(removeDecls)
import RmClasses(rmClasses)
import SccModule(sccTopDecls)
import Type(typeTopDecls)
import TypeLib(typeOfMain)
import Export(export,strExport)
import FixSyntax(fixSyntax)
import FreeVar(freeVar)
import Lift(liftCode)
import Case(caseTopLevel)
import PrimCode(primCode)

import STGArity(stgArity)
import STGGcode(stgGcode)
import PosAtom(posAtom)
import Gcode(Gcode,strGcode,strGcodeRel)
import GcodeFix(gcodeFixInit,gcodeFix,gcodeFixFinish)
import GcodeMem(gcodeMem)
import GcodeOpt1(gcodeOpt1)
import GcodeOpt2(gcodeOpt2)
import GcodeRel(gcodeRel)
import GcodeLow(gcodeHeader,gcodeDump)
import GcodeLowC(gcodeCHeader,gcodeGather)
import GcodeSpec(gcodeZCon)
import Depend(depend)
import PackedString(unpackPS)

import Foreign (Foreign,strForeign)
import ReportImports

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#endif
#if defined(__GLASGOW_HASKELL__)
import IOExts (trace)
#endif


--import NonStdProfile
profile a b = b

gmain cml = main' (words cml)

primFlags = (False   -- bool is not the same as Word
	    ,False   -- && || not is not primitive
	    ,False   -- translate into prim only when strict
	    )


main =
  do  args <- getArgs
      main' args

main' args =
  let  (rootdir,filename) = fixRootDir isUnix sourcefile
       flags = getFlags args
       isUnix = sUnix ((),flags)
       (realfile,sourcefile,typefile,cfile) =
          case getFiles args of
                [sourcefile] -> (sourcefile,sourcefile,fixTypeFile isUnix rootdir filename,fixObjectFile isUnix rootdir filename)
                [sourcefile,typefile,cfile] -> (sourcefile,sourcefile,typefile,cfile)
                [realfile,sourcefile,typefile,cfile] -> (realfile,sourcefile,typefile,cfile)
                _      -> error ("\nusage: nhc98comp file.hs\n       nhc98comp sourcefile interfacefile C-file\n       nhc98comp sourcefile sourcename interface C-file\n")
  in nhcLexParse ((realfile,sourcefile,typefile,cfile,rootdir:getIncludes args,getPreludes args),flags)
                  realfile


nhcLexParse flags filename = 
  profile "parse" $
  catch (readFile filename) (can'tOpen filename) >>= \mainChar ->
    let lexdata = lexical (sUnderscore flags)
                          (sSourceFile flags)
                          (if sUnlit flags then unlit (sSourceFile flags) mainChar else mainChar)
    in pF (sLex flags) "Lexical" (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata)) >>
       nhcNeed flags (parseit parseProg lexdata)

nhcNeed flags (Left err) = errorMsg (sSourceFile flags) (showErr err)
nhcNeed flags (Right (parsedProg@(Module pos (Visible modid) e impdecls inf d))) =
-- Insert check that sPart flags or modid == sourcefile
  profile "need" $
  pF (sParse flags) "Parse" (ppModule False dummyIntState parsedProg 0) >>
  let parsedProg' = dbgAddImport (sDbgTrans flags || sDbgPrelude flags) parsedProg in
  case needProg flags parsedProg' inf of
       (need,qualFun,overlap,Left err) -> errorMsg (sSourceFile flags) err
       (need,qualFun,overlap,Right (expFun,imports)) ->
         pF (sNeed flags) "Need (after reading source module)"  (show (treeMapList (:) need)) >>
         nhcImport flags modid qualFun expFun parsedProg' (initIS need) overlap imports


nhcImport flags modidl qualFun expFun parseProg importState overlap [] =
  profile "import []" $
  case getErrorsIS importState of
    (importState,errors) ->
 	if null errors
        then
                pF (sINeed flags) "Need after all imports"    (show (treeMapList (:)  (thd3 (getNeedIS importState)))) >>
                pF (sIBound flags) "Symbol table after import"  (mixLine (map show (treeMapList (:) (getSymbolTableIS importState)))) >>
                pF (sIRename flags) "Rename table after import"  (mixLine (map show (treeMapList (:) (getRenameTableIS importState)))) >>
		nhcRename flags modidl qualFun expFun parseProg importState overlap
	else
	        pF (True) "Error after import " (mixLine errors) >>
                pF (sINeed flags) "Need after all imports"    (show (treeMapList (:)  (thd3 (getNeedIS importState)))) >>
                pF (sIBound flags) "Symbol table after import"  (mixLine (map show (treeMapList (:) (getSymbolTableIS importState)))) >>
                pF (sIRename flags) "Rename table after import"  (mixLine (map show (treeMapList (:) (getRenameTableIS importState)))) >>
		exit

nhcImport flags modidl qualFun expFun parseProg importState overlap (x:xs) = 
  profile ("import:" ++ (reverse . show . fst3) x) $
 -- trace ("import:" ++ (reverse . show . fst3) x) $
    importOne flags importState x >>= \ importState ->
    pF (sIINeed flags) "Intermediate need after import"
			(show (treeMapList (:)  (thd3 (getNeedIS importState)))) >>
    pF (sIIBound flags) "Intermediate symbol table after import"
			(mixLine (map show (treeMapList (:) (getSymbolTableIS importState)))) >>
    nhcImport flags modidl qualFun expFun parseProg importState overlap xs

nhcRename flags modidl qualFun expFun (Module pos (Visible mrps) e impdecls inf decls) importState overlap =
  profile "rename" $
  case rename flags mrps qualFun expFun inf decls importState overlap of
    Left err ->
       pF True "Error when renaming:" (mixLine err) >>
       exit
    Right (decls,intState,tidFun,tidFunSafe,derived,userDefault,rt) ->
       case (getErrors intState) of
	 (intState,[]) ->
           depend flags intState rt >>
           pF (sRename flags) "Declarations after rename and fixity:" (ppDecls False intState decls 0) >>
           pF (sRBound flags) "Symbol table after rename and fixity:"  (mixLine (map show (treeMapList (:) (getSymbolTable intState)))) >>
	   nhcDbgDataTrans flags modidl mrps expFun userDefault tidFun tidFunSafe intState importState derived impdecls decls
	 (intState,errors) ->
     	   pF (True) "Error after rename " (mixLine errors) >>
	   exit

nhcDbgDataTrans flags modidl  mrps  expFun userDefault tidFun tidFunSafe intState importState derived impdecls decls =
  let (decls', derived', intState', constrs) = dbgDataTrans flags intState (error "repTree") tidFun derived decls 
  in pF (sDbg flags) "Abstract syntax tree after debug type transformation"
         (ppDecls False intState' decls' 0) >>
     nhcDerive flags modidl mrps expFun userDefault tidFun tidFunSafe intState' derived' impdecls decls' constrs

nhcDerive flags modidl  mrps  expFun userDefault tidFun tidFunSafe intState derived impdecls decls constrs =
  profile "derive" $
  nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe constrs 
             impdecls (derive tidFun intState derived decls)

nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe constrs impdecls (Left errors) =
  pF (True) "Deriving failed:" (mixLine errors) >>
  exit
nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe constrs impdecls (Right (intState,decls)) =
  profile "extract" $
  pF (sDerive flags) "Declarations after deriving:" (ppDecls False intState decls 0) >>
  pF (sDBound flags) "Symbol table after deriving:"  (mixLine (map show (treeMapList (:) (getSymbolTable intState)))) >>
  nhcDbgTrans flags modidl  mrps expFun userDefault tidFun tidFunSafe decls constrs impdecls (extract decls intState)

nhcDbgTrans flags modidl mrps expFun userDefault tidFun tidFunSafe decls constrs impdecls state =
  profile "dbgtrans" $
  case getErrors state of
    (state,[]) ->
      pF (sEBound flags) "Symbol table after extract:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      nhcRemove flags modidl  mrps expFun userDefault tidFun tidFunSafe 
          (if sDbgTrans flags then debugTrans flags state tidFun modidl (sSourceFile flags) impdecls decls constrs else (decls, state, Nothing))
    (state,errors) ->
      pF (True) "Error after extract:" (mixLine errors) >>
      exit

nhcRemove flags modidl  mrps expFun userDefault tidFun tidFunSafe (decls, state, sridt) =
  profile "remove" $
  pF (sDbg2 flags) "DbgTrans" (ppDecls False state decls 0) >>
  nhcScc flags modidl  mrps  expFun userDefault tidFun tidFunSafe sridt (removeDecls decls tidFun state)

nhcScc flags modidl  mrps expFun userDefault tidFun tidFunSafe sridt (decls,zcon,state) =
  profile "scc" $
  case getErrors state of
    (state,[]) ->
      pF (sRemove flags) "Declarations after remove fields:" (ppDecls False state decls 0) >>
      case rmClasses tidFun state decls of
	(code,decls,state) ->
	  nhcType flags zcon modidl  mrps  expFun userDefault tidFun tidFunSafe state code sridt (sccTopDecls decls)
    (state,errors) ->
      pF (True) "Error after remove fileds:" (mixLine errors) >>
      exit

nhcType flags zcon modidl  mrps  expFun userDefault tidFun tidFunSafe intState code sridt decls =
  profile "type" $
  pF (sScc flags) "Declarations after scc:" (ppDecls False intState decls 0) >>
  pF (sScc flags) "Class/instances after scc:" (ppClassCodes False intState code 0) >>
  nhcInterface flags zcon modidl  mrps expFun tidFun tidFunSafe sridt (typeTopDecls tidFun userDefault intState code (sDbgTrans flags) decls)


nhcInterface flags zcon modidl  mrps expFun  tidFun tidFunSafe sridt (code,decls,state) =
  let mod = reverse (unpackPS modidl) in
  profile "interface" $
  case getErrors state of
    (state,[]) -> 
      pF (sType flags) "Declarations after type deriving:" (ppDecls False state decls 0) >>
      pF (sTBound flags) "Symbol table after type deriving:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      pF (sRImport flags) ("Actual imports used by this module ("++mod++"):")  (mixLine (reportFnImports mod state)) >>
      (if reverse (unpackPS modidl) == "Main" then typeOfMain flags tidFun decls state else return state) >>= \ state ->
      nhcWriteI flags modidl mrps expFun tidFun state (export flags state) >>
      nhcFixSyntax flags zcon tidFun code sridt (fixSyntax decls state tidFun)
    (state,errors) ->
      pF (True) "Error after type deriving/checking" (mixLine errors) >>
      pF (sType flags) "Declarations after type deriving:" (ppDecls False state decls 0) >>
      pF (sTBound flags) "Symbol table after type deriving:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      exit

nhcWriteI flags modidl mrps expFun tidFun state expInfos =
  profile "write interface" $
  catch (writeFile (sTypeFile flags) (strExport (sPrelude flags) modidl state expInfos))
        (\ioerror -> hPutStr stderr ("Couldn't write interface file "++sTypeFile flags++":" ++ show ioerror ++ "\n") >> exit)

nhcFixSyntax flags zcon tidFun code sridt (decls,state,t2i) =
  profile "fixsyntax" $
      pF (sFixSyntax flags) "Declarations after fixSyntax"
			(mixLine (map (\ d -> ppDecl False state d 0) decls)) >>
      pF (sFSBound flags) "Symbol table after fixSyntax:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      nhcCase flags zcon tidFun sridt (caseTopLevel
                                         (if sPrelude flags
                                         then "Prelude:"++ sSourceFile flags
                                         else reverse (unpackPS (mrpsIS state)))
                                         t2i code decls state tidFun)

nhcCase flags zcon tidFun sridt (decls,state) =
  profile "case" $
  case getErrors state of
    (state,errors) ->
      pF (not (null errors)) "Warning pattern removal" (mixLine errors) >>
      pF (sCase flags) "Declarations after case:"  (strPCode (strISInt state) decls) >>
      pF (sCBound flags) "Symbol table after case:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      nhcPrim flags tidFun zcon sridt (primCode primFlags tidFun state decls)

nhcPrim flags tidFun zcon sridt (decls,state) =
  profile "prim" $
      pF (sPrim flags) "Declarations after prim expand:" (strPCode (strISInt state) decls) >>
      pF (sPBound flags) "Symbol table after prim expand:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
      nhcFree flags tidFun zcon sridt (freeVar (sKeepCase flags) decls state)

nhcFree flags tidFun zcon sridt (decls,state) =
  profile "free" $
     pF (sFree flags) "Declarations with explicit free variables:" (strPCode (strISInt state) decls) >>
     nhcCode1a flags tidFun zcon sridt (stgArity state decls)

nhcCode1a flags tidFun zcon sridt (decls,state) =
     pF (sArity flags) "Declarations after first arity grouping" (strPCode (strISInt state) decls) >>
     nhcLift flags tidFun zcon sridt (liftCode decls state tidFun)

nhcLift flags tidFun zcon sridt (decls,state) =
  profile "lift" $
     pF (sLift flags) "Declarations after lambda lifting:" (strPCode (strISInt state) decls) >>
     pF (sLBound flags) "Symbol table after lambda lifting:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
     nhcCode1b flags tidFun zcon sridt (stgArity state decls)

nhcCode1b flags tidFun zcon sridt (decls,state) =
     pF (sArity flags) "Declarations after second arity grouping" (strPCode (strISInt state) decls) >>
     nhcAtom flags zcon sridt (posAtom state decls)

nhcAtom flags zcon sridt (decls,state) =
  profile "atom" $
     pF (sAtom flags) "Declarations after atom:" (strPCode (strISInt state) decls) >>
     pF (sABound flags) "Symbol table after atom:"  (mixLine (map show (treeMapList (:) (getSymbolTable state)))) >>
     dumpZCon flags state (gcodeZCon (sProfile flags) state zcon) sridt decls

dumpZCon flags state zcons sridt decls =
  profile "dump zcon" $
     catch (openFile (sObjectFile flags) WriteMode) 
	   (\ioerror -> hPutStr stderr ("Couldn't open object file "++sObjectFile flags++":" ++ show ioerror ++ "\n") >> exit) >>= \ handle ->
     ( if (sAnsiC flags) then
         let es = dbgDumpSRIDTableC handle state flags sridt startEmitState in
         return (foldr (\a b-> gcodeGather state b a) es zcons)
       else
         dbgDumpSRIDTable handle state flags sridt >>
         catch (hPutStr handle (gcodeHeader (foldr ( \ a b -> foldr (gcodeDump state) b a) "\n" zcons)))
               (\ioerror -> hPutStr stderr ("Failed writing to object file "++sObjectFile flags++":" ++ show ioerror ++ "\n") >> exit) >>
         return startEmitState) >>= \es->
     profile "dump code" $
     dumpCode handle flags [] (gcodeFixInit state flags) es decls

dumpCode handle flags foreigns (state,fixState) es [] =
     dumpCodeEnd handle flags state es foreigns (gcodeFixFinish state fixState)

dumpCode handle flags foreigns (state,fixState) es (decl:decls) =
  -- profile "dump code" $
     nhcCode2 handle flags fixState decls es foreigns (stgGcode (sProfile flags) state decl)

 
nhcCode2 handle flags fixState decls es foreigns (gcode,state,newforeigns) =
     pF (sGcode flags) "G Code" (concatMap (strGcode state) ( gcode)) >>
     nhcCode3 handle flags decls es (foreigns++newforeigns) (gcodeFix flags state fixState gcode)

nhcCode3 handle flags decls es foreigns (state,fixState,gcode) =
     pF (sGcodeFix flags) "G Code (fixed)" (concatMap (strGcode state) ( gcode)) >>
     nhcCode35 handle flags fixState decls es foreigns (gcodeOpt1 state gcode)
 
nhcCode35 handle flags fixState decls es foreigns (gcode,state) =
     pF (sGcodeOpt1 flags) "G Code (opt1)" (concatMap (strGcode state) ( gcode)) >>
     nhcCode4 handle flags fixState decls es foreigns (gcodeMem (sProfile flags) state gcode)
 
nhcCode4 handle flags fixState decls es foreigns (gcode,state) =
     pF (sGcodeMem flags) "G Code (mem)" (concatMap (strGcode state) ( gcode)) >>
     nhcCode5 handle flags fixState decls es foreigns (gcodeOpt2 state gcode)
 
nhcCode5 handle flags fixState decls es foreigns (gcode,state) =
     pF (sGcodeOpt2 flags) "G Code (opt2)" (concatMap (strGcode state) ( gcode)) >>
     nhcCode6 handle flags fixState decls state es foreigns (gcodeRel gcode)
 
nhcCode6 handle flags fixState decls state es foreigns gcode =
     pF (sGcodeRel flags) "G Code (rel)" (concatMap (strGcodeRel state) gcode) >>
    (if (sAnsiC flags) then
       return (gcodeGather state es gcode)
     else
       catch (hPutStr handle (foldr (gcodeDump state) "\n" gcode))
             (\ioerror -> hPutStr stderr ("Failed appending to object file "++sObjectFile flags++":" ++ show ioerror ++ "\n") >> exit) >>
       return es) >>= \es'->
     dumpCode handle flags foreigns (state,fixState) es' decls

dumpCodeEnd handle flags state es foreigns gcode =
  profile "dump tables" $
    pF (sGcodeRel flags) "G Code (rel)" (concatMap (strGcodeRel state) (concat gcode)) >>
    (if (sAnsiC flags) then
       (return (foldr (\a b-> gcodeGather state b a) es gcode)) >>= \es'->
       catch (hPutStr handle (gcodeCHeader (emitState es')))
             (\ioerror -> hPutStr stderr ("Failed writing code to object file "++sObjectFile flags++":" ++ show ioerror ++ "\n") >> exit)
     else
       catch (hPutStr handle (foldr (\a b -> foldr (gcodeDump state) b a) "\n" gcode))
             (\ioerror -> hPutStr stderr ("Failed appending tables to object file "++sObjectFile flags++":" ++ show ioerror ++ "\n") >> exit)
    ) >>
    (if null foreigns then
         return ()
      else
         hPutStr handle "\n#include <haskell2c.h>\n" >>
         mapM_ (\f-> hPutStr handle (strForeign f "")) foreigns
    ) >>
    hClose handle


---   Small help functions

strISInt state v = strIS state v ++ "{"++show v++"}"
