{- ---------------------------------------------------------------------------
The function main calls all the transformation phases of the compiler.
It does lots of head-standing to try to ensure
that things happen in the right order (due to lazy evaluation), that
error messages get reported correctly, and to be as space-efficient as
possible.
-}
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
import Extra(Pos(..),mix,mixSpace,jRight,jLeft,noPos,strPos,showErr,mixLine,
             pair,fst3,thd3)
import State(State0(..))
import ImportState(ImportState,Info,IE,initIS,getErrorsIS,getSymbolTableIS
                  ,getRenameTableIS)
import IntState(IntState,dummyIntState,getSymbolTable,getErrors,strIS,mrpsIS)
import PPLib
import NeedLib(initNeed)
import RenameLib(getSymbolTableRS,RenameState,getErrorsRS)
import PreImport
import ParseCore(Parser(..),ParseBad(..),ParseError(..),ParseGood(..),
                 ParseResult(..),parseit)

import Flags
{- correct import list does not work with current nhc 
            (Flags,processArgs,pF
            ,sRealFile,sProfile,sUnix,sUnlit,sSourceFile,sUnderscore,sLex
            ,sDbgPrelude,sDbgTrans,sNeed,sParse,sIRename,sIBound,sINeed
            ,sIIBound,sIINeed,sRBound,sRename,sDbg,sDBound,sDerive,sEBound
            ,sDbg2,sRemove,sScc,sRImport,sTBound,sType,sTypeFile,sPrelude
            ,sFSBound,sFixSyntax,sCBound,sCase,sKeepCase,sPBound,sPrim,sFree
            ,sArity,sLBound,sLift,sABound,sAtom,sAnsiC,sObjectFile
            ,sGcode,sGcodeFix,sGcodeOpt1,sGcodeMem,sGcodeOpt2,sGcodeRel)
-}
import StrSyntax(strType,StrId(..))
import SyntaxPos	-- DW
import PPSyntax(ppModule,ppDecl,ppDecls,ppImpDecls,ppInterface,ppFun,
                ppClassCodes)
import StrPos(strPCode)

import TokenId(TokenId(..),t_Arrow,t_List,tPrelude,tminus,tnegate,tTrue)
import IdKind(IdKind(..))
import Lex(Lex,LexAnnot)  -- need show

import Unlit(unlit)
import Lexical(PosToken(..),PosTokenPre(..),LexState(..),lexical)
import Parse(parseProg)
import Need(needProg)
import Overlap(Overlap,Resolution)
import Import(HideDeclIds,importOne)
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
import Export(buildInterface)
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
import PackedString(PackedString, unpackPS)

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


main' :: [String] -> IO ()
main' args = nhcLexParse flags (sRealFile flags)
  where
  flags = processArgs args


{- lex and parse source code -}
nhcLexParse :: Flags -> [Char] -> IO () 

nhcLexParse flags filename = profile "parse" $ do
  mainChar <- catch (readFile filename) (can'tOpen filename) 
  let lexdata = lexical (sUnderscore flags) (sSourceFile flags)
                  (if sUnlit flags 
                     then unlit (sSourceFile flags) mainChar 
                     else mainChar)
  pF (sLex flags) "Lexical" 
     (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata)) 
  nhcNeed flags (parseit parseProg lexdata)


{- 
Perform "need" analysis (what imported entities are required?) 
Second argument may contain error message or parse tree
Creates ImportState for next pass.
-}
nhcNeed :: Flags -> Either (Pos,String,[String]) (Module TokenId) -> IO () 

nhcNeed flags 
        (Left err) = errorMsg (sSourceFile flags) (showErr err)
nhcNeed flags 
        (Right (parsedProg@(Module pos (Visible modid) e impdecls inf d))) =
  -- Insert check that sPart flags or modid == sourcefile
  profile "need" $ do
    pF (sParse flags) "Parse" (ppModule False dummyIntState parsedProg 0) 
    let parsedProg' = 
          dbgAddImport (sDbgTrans flags || sDbgPrelude flags) parsedProg 
    case needProg flags parsedProg' inf of
      (need,qualFun,overlap,Left err) -> errorMsg (sSourceFile flags) err
      (need,qualFun,overlap,Right (expFun,imports)) -> do
         pF (sNeed flags) "Need (after reading source module)"  
            (show (treeMapList (:) need)) 
         nhcImport flags modid qualFun expFun parsedProg' (initIS need) 
                   overlap imports


{-
Parse interface files for imported modules 
-}
nhcImport :: Flags 
          -> PackedString 
          -> (TokenId -> [TokenId]) 
          -> (Bool -> Bool -> TokenId -> IdKind -> IE) 
          -> Module TokenId 
          -> ImportState 
          -> Overlap 
          -> [(PackedString
              ,   (PackedString,PackedString,Tree (TokenId,IdKind)) 
               -> [[TokenId]] 
               -> Bool
              ,HideDeclIds
              )] 
          -> IO ()

nhcImport flags modidl qualFun expFun parseProg importState overlap [] =
  profile "import []" $
  case getErrorsIS importState of
    (importState,errors) ->
      if null errors
        then do
          pF (sINeed flags) "Need after all imports"    
             (show (treeMapList (:)  (thd3 (getNeedIS importState)))) 
          pF (sIBound flags) "Symbol table after import"  
             (mixLine (map show (treeMapList 
                                   (:) (getSymbolTableIS importState)))) 
          pF (sIRename flags) "Rename table after import"  
             (mixLine (map show (treeMapList 
                                   (:) (getRenameTableIS importState)))) 
	  nhcRename flags modidl qualFun expFun parseProg importState overlap
	else do
	  pF (True) "Error after import " (mixLine errors) 
          pF (sINeed flags) "Need after all imports"    
             (show (treeMapList (:)  (thd3 (getNeedIS importState)))) 
          pF (sIBound flags) "Symbol table after import"  
             (mixLine (map show (treeMapList 
                                  (:) (getSymbolTableIS importState)))) 
          pF (sIRename flags) "Rename table after import"  
             (mixLine (map show (treeMapList 
                                  (:) (getRenameTableIS importState)))) 
	  exit

nhcImport flags modidl qualFun expFun parseProg importState overlap (x:xs) = 
  profile ("import:" ++ (reverse . show . fst3) x) $ do
  -- trace ("import:" ++ (reverse . show . fst3) x) $
    importState <- importOne flags importState x 
    pF (sIINeed flags) "Intermediate need after import"
       (show (treeMapList (:)  (thd3 (getNeedIS importState))))
    pF (sIIBound flags) "Intermediate symbol table after import"
       (mixLine (map show (treeMapList (:) (getSymbolTableIS importState))))
    nhcImport flags modidl qualFun expFun parseProg importState overlap xs
    

{-
Rename identifiers (also patches fixity information)
Changes from importState to intState
-}
nhcRename :: Flags 
          -> PackedString 
          -> (TokenId -> [TokenId]) 
          -> (Bool -> Bool -> TokenId -> IdKind -> IE) 
          -> Module TokenId 
          -> ImportState 
          -> Overlap 
          -> IO ()   

nhcRename flags modidl qualFun expFun (Module pos (Visible mrps) e impdecls inf decls) importState overlap =
  profile "rename" $
  case rename flags mrps qualFun expFun inf decls importState overlap of
    Left err -> do
                  pF True "Error when renaming:" (mixLine err) 
                  exit
    Right (decls,intState,tidFun,tidFunSafe,derived,userDefault,rt) ->
       case (getErrors intState) of
	 (intState,[]) -> do
           depend flags intState rt 
           pF (sRename flags) "Declarations after rename and fixity:" 
              (ppDecls False intState decls 0) 
           pF (sRBound flags) "Symbol table after rename and fixity:"  
              (mixLine (map show (treeMapList (:) (getSymbolTable intState))))
	   nhcDbgDataTrans flags modidl mrps expFun userDefault tidFun 
             tidFunSafe intState importState derived impdecls decls
	 (intState,errors) -> do
     	   pF (True) "Error after rename " (mixLine errors) 
	   exit


{-
Debugging source-to-source translation of data type definitions.
For tracing only.
-} 
nhcDbgDataTrans :: Flags 
                -> PackedString 
                -> a {- PackedString -} 
                -> b {- Bool -> Bool -> TokenId -> IdKind -> IE -}
                -> Maybe [Int] 
                -> ((TokenId,IdKind) -> Int) 
                -> c {- (TokenId,IdKind) -> Maybe Int -}
                -> IntState 
                -> d {- ImportState -} 
                -> [(Int,[(Pos,Int)])] 
                -> [ImpDecl TokenId] 
                -> Decls Int 
                -> IO ()  

nhcDbgDataTrans flags modidl mrps expFun userDefault tidFun tidFunSafe 
  intState importState derived impdecls decls =
  let (decls', derived', intState', constrs) = 
        dbgDataTrans flags intState (error "repTree") tidFun derived decls 
  in do
    pF (sDbg flags) "Abstract syntax tree after debug type transformation"
       (ppDecls False intState' decls' 0) 
    nhcDerive flags modidl mrps expFun userDefault tidFun tidFunSafe 
      intState' derived' impdecls decls' constrs


{-
Derive class instances where required by data definitions
-}
nhcDerive :: Flags 
          -> PackedString 
          -> a 
          -> b 
          -> Maybe [Int] 
          -> ((TokenId,IdKind) -> Int) 
          -> c 
          -> IntState 
          -> [(Int,[(Pos,Int)])] 
          -> [ImpDecl TokenId] 
          -> Decls Int 
          -> Maybe [(Pos,Int)] 
          -> IO ()

nhcDerive flags modidl  mrps  expFun userDefault tidFun tidFunSafe 
  intState derived impdecls decls constrs =
  profile "derive" $
  nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe constrs 
    impdecls (derive tidFun intState derived decls)

{-
Extract what?
-}
nhcExtract :: Flags 
           -> PackedString 
           -> a 
           -> b 
           -> Maybe [Int] 
           -> ((TokenId,IdKind) -> Int) 
           -> c 
           -> Maybe [(Pos,Int)] 
           -> [ImpDecl TokenId] 
           -> Either [String] (IntState,Decls Int) 
           -> IO ()

nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe 
  constrs impdecls (Left errors) = do
    pF (True) "Deriving failed:" (mixLine errors) 
    exit
nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe 
  constrs impdecls (Right (intState,decls)) =
    profile "extract" $ do
    pF (sDerive flags) "Declarations after deriving:" 
       (ppDecls False intState decls 0) 
    pF (sDBound flags) "Symbol table after deriving:"  
       (mixLine (map show (treeMapList (:) (getSymbolTable intState)))) 
    nhcDbgTrans flags modidl  mrps expFun userDefault tidFun tidFunSafe 
      decls constrs impdecls (extract decls intState)

{-
Debugging source-to-source translation of function definitions 
(for tracing only)
-}
nhcDbgTrans :: Flags 
            -> PackedString 
            -> a 
            -> b 
            -> Maybe [Int] 
            -> ((TokenId,IdKind) -> Int) 
            -> c 
            -> Decls Int 
            -> Maybe [(Pos,Int)] 
            -> [ImpDecl TokenId] 
            -> IntState 
            -> IO () 

nhcDbgTrans flags modidl mrps expFun userDefault tidFun tidFunSafe decls 
  constrs impdecls state =
    profile "dbgtrans" $
    case getErrors state of
      (state,[]) -> do
        pF (sEBound flags) "Symbol table after extract:"  
           (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
        nhcRemove flags modidl  mrps expFun userDefault tidFun tidFunSafe 
          (if sDbgTrans flags 
             then debugTrans flags state tidFun modidl (sSourceFile flags) 
                    impdecls decls constrs 
             else (decls, state, Nothing))
      (state,errors) -> do
        pF (True) "Error after extract:" (mixLine errors) 
        exit


{-
Translate list comprehensions, do notation, etc, to core)
-} 
nhcRemove :: Flags 
          -> PackedString 
          -> a 
          -> b 
          -> Maybe [Int] 
          -> ((TokenId,IdKind) -> Int) 
          -> c 
          -> (Decls Int,IntState,Maybe ((Int,[Int]),[(Pos,Int)]
                                       ,[ImpDecl TokenId],String)) 
          -> IO () 

nhcRemove flags modidl  mrps expFun userDefault tidFun tidFunSafe 
  (decls, state, sridt) =
  profile "remove" $ do
  pF (sDbg2 flags) "DbgTrans" (ppDecls False state decls 0) 
  nhcScc flags modidl mrps expFun userDefault tidFun tidFunSafe sridt 
    (removeDecls decls tidFun state)


{- 
Strongly Connected Component analysis 
-}
nhcScc :: Flags 
       -> PackedString 
       -> a 
       -> b 
       -> Maybe [Int] 
       -> ((TokenId,IdKind) -> Int) 
       -> c 
       -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
       -> (Decls Int,[Int],IntState) 
       -> IO ()

nhcScc flags modidl  mrps expFun userDefault tidFun tidFunSafe sridt 
  (decls,zcon,state) =
  profile "scc" $
  case getErrors state of
    (state,[]) -> do
      pF (sRemove flags) "Declarations after remove fields:" 
         (ppDecls False state decls 0) 
      case rmClasses tidFun state decls of
	(code,decls,state) ->
	  nhcType flags zcon modidl  mrps  expFun userDefault tidFun 
            tidFunSafe state code sridt (sccTopDecls decls)
    (state,errors) -> do
      pF (True) "Error after remove fileds:" (mixLine errors)
      exit

{- 
Type inference 
-}
nhcType :: Flags 
        -> [Int] 
        -> PackedString 
        -> a 
        -> b 
        -> Maybe [Int]  -- types chosen by user for defaulting of Num classes
        -> ((TokenId,IdKind) -> Int) 
        -> c 
        -> IntState 
        -> [ClassCode (Exp Int) Int] 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> Decls Int 
        -> IO ()

nhcType flags zcon modidl  mrps  expFun userDefault tidFun tidFunSafe 
  intState code sridt decls =
  profile "type" $ do
  pF (sScc flags) "Declarations after scc:" (ppDecls False intState decls 0)
  pF (sScc flags) "Class/instances after scc:" 
    (ppClassCodes False intState code 0)
  nhcInterface flags zcon modidl  mrps expFun tidFun tidFunSafe sridt 
    (typeTopDecls tidFun userDefault intState code (sDbgTrans flags) decls)


{- 
Build interface file for this module and write it out 
-}
nhcInterface :: Flags 
             -> [Int]  
             -> PackedString 
             -> a 
             -> b 
             -> ((TokenId,IdKind) -> Int) 
             -> c 
             -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
             -> ([ClassCode (Exp Int) Int],Decls Int,IntState) 
             -> IO ()

nhcInterface flags zcon modidl mrps expFun tidFun tidFunSafe sridt 
  (code,decls,state) =
  let mod = reverse (unpackPS modidl) in
  profile "interface" $
  case getErrors state of
    (state,[]) -> do
      pF (sType flags) "Declarations after type deriving:" 
         (ppDecls False state decls 0) 
      pF (sTBound flags) "Symbol table after type deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
      pF (sRImport flags) ("Actual imports used by this module ("++mod++"):") 
         (mixLine (reportFnImports mod state)) 
      state <- if mod == "Main" 
                 then typeOfMain flags tidFun decls state 
                 else return state
      nhcWriteI flags (buildInterface flags modidl state) 
      nhcFixSyntax flags zcon tidFun code sridt (fixSyntax decls state tidFun)
    (state,errors) -> do
      pF (True) "Error after type deriving/checking" (mixLine errors)
      pF (sType flags) "Declarations after type deriving:" 
         (ppDecls False state decls 0) 
      pF (sTBound flags) "Symbol table after type deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state))))
      exit


{- 
Write interface file (used by preceeding definition)
-}
nhcWriteI :: Flags 
          -> String -- content of the interface file
          -> IO ()

nhcWriteI flags interfaceContent =
  profile "write interface" $
  catch (writeFile (sTypeFile flags) interfaceContent)
        (\ioerror -> do
                       hPutStr stderr ("Couldn't write interface file "
                                       ++ sTypeFile flags ++ ":" 
                                       ++ show ioerror ++ "\n") 
                       exit)


{- 
Fix syntax (small tweaks based on type information) 
optimisation: evaluation of `fromInteger' where possible
(actually done by preceding function)
-}
nhcFixSyntax :: Flags 
             -> [Int] 
             -> ((TokenId,IdKind) -> Int) 
             -> [ClassCode (Exp Int) Int] 
             -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
             -> ([Decl Int],IntState,Tree (TokenId,Int)) 
             -> IO ()

nhcFixSyntax flags zcon tidFun code sridt (decls,state,t2i) =
  profile "fixsyntax" $ do
  pF (sFixSyntax flags) "Declarations after fixSyntax"
     (mixLine (map (\ d -> ppDecl False state d 0) decls)) 
  pF (sFSBound flags) "Symbol table after fixSyntax:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state))))
  nhcCase flags zcon tidFun sridt 
    (caseTopLevel (if sPrelude flags
                     then "Prelude:"++ sSourceFile flags
                     else reverse (unpackPS (mrpsIS state)))
                  t2i code decls state tidFun)


{-
Remove pattern matching: Change all pattern matches to case expressions.
(actually done by preceding function)
-}
nhcCase :: Flags 
        -> [Int] 
        -> ((TokenId,IdKind) -> Int) 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcCase flags zcon tidFun sridt (decls,state) =
  profile "case" $
  case getErrors state of
    (state,errors) -> do
      pF (not (null errors)) "Warning pattern removal" (mixLine errors) 
      pF (sCase flags) "Declarations after case:"  
         (strPCode (strISInt state) decls) 
      pF (sCBound flags) "Symbol table after case:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state))))
      nhcPrim flags tidFun zcon sridt (primCode primFlags tidFun state decls)


{-
Expand primitives ?
(actually done by preceding function)
-}
nhcPrim :: Flags 
        -> ((TokenId,IdKind) -> Int) 
        -> [Int] 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcPrim flags tidFun zcon sridt (decls,state) =
  profile "prim" $ do
  pF (sPrim flags) "Declarations after prim expand:" 
     (strPCode (strISInt state) decls) 
  pF (sPBound flags) "Symbol table after prim expand:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
  nhcFree flags tidFun zcon sridt (freeVar (sKeepCase flags) decls state)


{-
Determine free variables (for lambda lifting)
(actually done by preceding function)
-}
nhcFree :: Flags 
        -> ((TokenId,IdKind) -> Int) 
        -> [Int] 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcFree flags tidFun zcon sridt (decls,state) =
  profile "free" $ do
  pF (sFree flags) "Declarations with explicit free variables:" 
     (strPCode (strISInt state) decls)
  nhcCode1a flags tidFun zcon sridt (stgArity state decls)


{-
Do arity grouping on declarations (for lambda lifting)
(actually done by preceding function)
-}
nhcCode1a :: Flags 
          -> ((TokenId,IdKind) -> Int) 
          -> [Int] 
          -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
          -> ([(Int,PosLambda)],IntState) 
          -> IO ()

nhcCode1a flags tidFun zcon sridt (decls,state) = do
  pF (sArity flags) "Declarations after first arity grouping" 
     (strPCode (strISInt state) decls) 
  nhcLift flags tidFun zcon sridt (liftCode decls state tidFun)

{-
Lambda lift
(actually done by preceding function)
-}
nhcLift :: Flags 
        -> a 
        -> [Int] 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcLift flags tidFun zcon sridt (decls,state) =
  profile "lift" $ do
  pF (sLift flags) "Declarations after lambda lifting:" 
     (strPCode (strISInt state) decls) 
  pF (sLBound flags) "Symbol table after lambda lifting:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
  nhcCode1b flags tidFun zcon sridt (stgArity state decls)
    
{-
Do arity grouping again
(actually done by preceding function)
-}
nhcCode1b :: Flags 
          -> a 
          -> [Int] 
          -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
          -> ([(Int,PosLambda)],IntState) 
          -> IO ()

nhcCode1b flags tidFun zcon sridt (decls,state) = do
  pF (sArity flags) "Declarations after second arity grouping" 
     (strPCode (strISInt state) decls) 
  nhcAtom flags zcon sridt (posAtom state decls)


nhcAtom :: Flags 
        -> [Int] 
        -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcAtom flags zcon sridt (decls,state) =
  profile "atom" $ do
  pF (sAtom flags) "Declarations after atom:" (strPCode (strISInt state) decls)
  pF (sABound flags) "Symbol table after atom:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state))))
  dumpZCon flags state (gcodeZCon (sProfile flags) state zcon) sridt decls


dumpZCon :: Flags 
         -> IntState 
         -> [[Gcode]] 
         -> Maybe ((Int,[Int]),[(Pos,Int)],[ImpDecl TokenId],String) 
         -> [(Int,PosLambda)] 
         -> IO ()

dumpZCon flags state zcons sridt decls =
  profile "dump zcon" $ do
  handle <- catch (openFile (sObjectFile flags) WriteMode) 
	          (\ioerror -> do
                                 hPutStr stderr ("Couldn't open object file "
                                   ++ sObjectFile flags ++ ":" 
                                   ++ show ioerror ++ "\n")  
                                 exit) 
  es <- if (sAnsiC flags) 
          then do
                 let es = dbgDumpSRIDTableC handle state flags sridt 
                            startEmitState 
                 return (foldr (\a b-> gcodeGather state b a) es zcons)
          else do
            dbgDumpSRIDTable handle state flags sridt 
            catch (hPutStr handle (gcodeHeader 
                     (foldr ( \ a b -> foldr (gcodeDump state) b a) 
                                             "\n" zcons)))
                  (\ioerror -> do
                                 hPutStr stderr 
                                    ("Failed writing to object file "
                                    ++ sObjectFile flags ++ ":" 
                                    ++ show ioerror ++ "\n")  
                                 exit) 
            return startEmitState
  profile "dump code" $
    dumpCode handle flags [] (gcodeFixInit state flags) es decls


dumpCode :: Handle 
         -> Flags 
         -> [Foreign] 
         -> (IntState,(Tree ((Int,Int),Int),(Tree ([Char],Int),[(Int,Gcode)])))
         -> EmitState 
         -> [(Int,PosLambda)] 
         -> IO ()

dumpCode handle flags foreigns (state,fixState) es [] =
     dumpCodeEnd handle flags state es foreigns (gcodeFixFinish state fixState)

dumpCode handle flags foreigns (state,fixState) es (decl:decls) =
  -- profile "dump code" $
  nhcCode2 handle flags fixState decls es foreigns 
    (stgGcode (sProfile flags) state decl)

 
nhcCode2 handle flags fixState decls es foreigns (gcode,state,newforeigns) = do
  pF (sGcode flags) "G Code" (concatMap (strGcode state) ( gcode)) 
  nhcCode3 handle flags decls es (foreigns++newforeigns) 
    (gcodeFix flags state fixState gcode)


nhcCode3 handle flags decls es foreigns (state,fixState,gcode) = do
  pF (sGcodeFix flags) "G Code (fixed)" (concatMap (strGcode state) ( gcode))
  nhcCode35 handle flags fixState decls es foreigns (gcodeOpt1 state gcode)
 

nhcCode35 handle flags fixState decls es foreigns (gcode,state) = do
  pF (sGcodeOpt1 flags) "G Code (opt1)" (concatMap (strGcode state) ( gcode))
  nhcCode4 handle flags fixState decls es foreigns 
    (gcodeMem (sProfile flags) state gcode)

 
nhcCode4 handle flags fixState decls es foreigns (gcode,state) = do
  pF (sGcodeMem flags) "G Code (mem)" (concatMap (strGcode state) ( gcode))
  nhcCode5 handle flags fixState decls es foreigns (gcodeOpt2 state gcode)
 

nhcCode5 handle flags fixState decls es foreigns (gcode,state) = do
  pF (sGcodeOpt2 flags) "G Code (opt2)" (concatMap (strGcode state) ( gcode))
  nhcCode6 handle flags fixState decls state es foreigns (gcodeRel gcode)
 

nhcCode6 handle flags fixState decls state es foreigns gcode = do
  pF (sGcodeRel flags) "G Code (rel)" (concatMap (strGcodeRel state) gcode)
  es' <- if (sAnsiC flags) 
           then return (gcodeGather state es gcode)
           else do 
                  catch (hPutStr handle (foldr (gcodeDump state) "\n" gcode))
                        (\ioerror -> do
                                       hPutStr stderr 
                                         ("Failed appending to object file "
                                          ++ sObjectFile flags ++ ":"  
                                          ++ show ioerror ++ "\n") 
                                       exit) 
                  return es 
  dumpCode handle flags foreigns (state,fixState) es' decls


dumpCodeEnd :: Handle 
            -> Flags 
            -> IntState 
            -> EmitState 
            -> [Foreign] 
            -> [[Gcode]] 
            -> IO ()

dumpCodeEnd handle flags state es foreigns gcode =
  profile "dump tables" $ do
  pF (sGcodeRel flags) "G Code (rel)" 
     (concatMap (strGcodeRel state) (concat gcode)) 
  if (sAnsiC flags) 
    then do
           let es' = foldr (\a b-> gcodeGather state b a) es gcode
           catch (hPutStr handle (gcodeCHeader (emitState es')))
                 (\ioerror -> do
                                hPutStr stderr 
                                  ("Failed writing code to object file "
                                    ++ sObjectFile flags ++ ":" 
                                    ++ show ioerror ++ "\n") 
                                exit)
    else catch (hPutStr handle (foldr (\a b -> foldr (gcodeDump state) b a) 
                                      "\n" gcode))
               (\ioerror -> do
                              hPutStr stderr 
                                ("Failed appending tables to object file "
                                 ++ sObjectFile flags ++ ":" ++ 
                                 show ioerror ++ "\n") 
                              exit)
  if null foreigns 
    then return ()
    else do
           hPutStr handle "\n#include <haskell2c.h>\n" 
           mapM_ (\f-> hPutStr handle (strForeign f "")) foreigns
  hClose handle


---   Small help functions

strISInt state v = strIS state v ++ "{"++show v++"}"

{- End Module Main ----------------------------------------------------------}
