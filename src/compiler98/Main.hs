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
             pair,fst3,thd3,trace)
import State(State0(..))
import ImportState(ImportState,Info,IE,initIS,getErrorsIS,getSymbolTableIS
                  ,getRenameTableIS)
import IntState(IntState,dummyIntState,getSymbolTable,getErrors,strIS,mrpsIS)
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
            ,sIIBound,sIINeed,sRBound,sRename,sTraceData,sDBound,sDerive
            ,sEBound
            ,sTraceFns,sRemove,sScc,sRImport,sTBound,sType,sTypeFile,sPrelude
            ,sFSBound,sFixSyntax,sCBound,sCase,sKeepCase,sPBound,sPrim,sFree
            ,sArity,sLBound,sLift,sABound,sAtom,sAnsiC,sObjectFile
            ,sGcode,sGcodeFix,sGcodeOpt1,sGcodeMem,sGcodeOpt2,sGcodeRel)
-}
import SyntaxPos	-- DW
import PrettySyntax(prettyPrintTokenId,prettyPrintId,prettyPrintTraceId
                   ,ppModule,ppTopDecls,ppClassCodes)
import StrPos(strPCode)

import TokenId(TokenId(..),t_Arrow,t_List,tPrelude,tminus,tnegate,tTrue)
import IdKind(IdKind(..))
import Id(Id)
import Lex(Lex,LexAnnot)  -- need show

import Unlit(unlit)
import Lexical(PosToken(..),PosTokenPre(..),LexState(..),lexical)
import Parse(parseProg)
import Need(needProg)
import Overlap(Overlap,Resolution)
import Import(HideDeclIds,importOne)
import IExtract(getNeedIS)
import Rename(rename)
import FFITrans(ffiTrans)
import DbgDataTrans(dbgDataTrans)
import DbgTrans(SRIDTable,debugTrans,dbgAddImport)
import DbgDumpSRIDTable(dbgDumpSRIDTable)
import DbgDumpSRIDTableC(dbgDumpSRIDTableC)
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
import AuxFile
import AuxLabelAST


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
nhcLexParse :: Flags -> String -> IO () 
nhcLexParse flags filename =
  profile "parse" $ do
  mainChar <- catch (readFile filename) (can'tOpen filename) 
  let lexdata = lexical (sUnderscore flags) (sSourceFile flags)
                  (if sUnlit flags 
                     then unlit (sSourceFile flags) mainChar 
                     else mainChar)
  pF (sLex flags) "Lexical" 
     (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata)) 
  nhcAux flags (parseit parseProg lexdata)


{-
-- Read and write auxiliary information files (for tracing).
-- Then relabel the syntax tree with the auxiliary information.
-- Eventually, the tracing transformation itself will also be in this
-- phase of the compiler.
-}
nhcAux :: Flags -> Either (Pos,String,[String]) (Module TokenId) -> IO () 
nhcAux flags (Left err) = errorMsg (sSourceFile flags) (showErr err)
nhcAux flags (Right parsedProg) = do
    if sHatAuxFile flags
      then do toAuxFile flags (sAuxFile flags) parsedProg
            --putStrLn (prettyPrintTokenId flags ppModule parsedProg)
              newprog <- auxLabelSyntaxTree flags parsedProg
              putStrLn (prettyPrintTraceId flags ppModule newprog)
              exitWith (ExitSuccess)
      else return ()
    nhcNeed flags parsedProg


{- 
-- Perform "need" analysis (what imported entities are required?) 
-- Second argument may contain error message or parse tree
-- Creates ImportState for next pass.
-}
nhcNeed :: Flags -> (Module TokenId) -> IO () 
nhcNeed flags (parsedProg@(Module pos (Visible modid) e impdecls inf d)) =
    -- Insert check that sPart flags or modid == sourcefile
    {-profile "need" $-}  do
    pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedProg) 
    let parsedProg' = 
          dbgAddImport (sDbgTrans flags || sDbgPrelude flags) parsedProg 
    case needProg flags parsedProg' inf of
      (need,qualFun,overlap,Left err) -> errorMsg (sSourceFile flags) err
      (need,qualFun,overlap,Right (expFun,imports)) -> do
         pF (sNeed flags) "Need (after reading source module)"  
            (show (treeMapList (:) need)) 
         profile "imports" $
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
  --profile "import []" $
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
  {-profile ("import:" ++ (reverse . show . fst3) x) $-}  do
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

nhcRename flags modidl qualFun expFun (Module pos (Visible mrps) e
          impdecls inf decls) importState overlap =
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
              (prettyPrintId flags intState ppTopDecls decls) 
           pF (sRBound flags) "Symbol table after rename and fixity:"  
              (mixLine (map show (treeMapList (:) (getSymbolTable intState))))

           -- **** note, tidFunSafe does not appear to be used anywhere!

           nhcFFItrans flags modidl mrps expFun userDefault tidFun 
             tidFunSafe intState derived impdecls decls {- constrs -}

	 (intState,errors) -> do
     	   pF (True) "Error after rename " (mixLine errors) 
	   exit

{-
For foreign imports, transform all functions with IO types to introduce the
appropriate wrapper.
-}
nhcFFItrans :: Flags 
          -> PackedString 
          -> a 
          -> b 
          -> Maybe [Id]         -- passes: user defaults for Num classes
          -> ((TokenId,IdKind) -> Id) 
             -- reads: mapping from id token and kind to internal id
          -> c 
          -> IntState           -- updates: internal compiler state
          -> [(Id,[(Pos,Id)])] 
             -- instances that have to be derived
             -- class , position where derived, type constructor
          -> [ImpDecl TokenId]  -- passes: import declarations of module
          -> Decls Id           -- updates: declarations of module
          -> IO ()

nhcFFItrans flags modidl mrps expFun userDefault tidFun tidFunSafe
            intState derived impdecls decls =
  case ffiTrans decls tidFun intState of
      (decls', intState') -> do
          nhcDerive flags modidl mrps expFun userDefault tidFun tidFunSafe
                    intState' derived impdecls decls'

{-
Derive class instances where required by data definitions
-}
nhcDerive :: Flags 
          -> PackedString 
          -> a 
          -> b 
          -> Maybe [Id]         -- passes: user defaults for Num classes
          -> ((TokenId,IdKind) -> Id) 
             -- reads: maping from id token and kind to internal id
          -> c 
          -> IntState           -- updates: internal compiler state
          -> [(Id,[(Pos,Id)])] 
             -- instances that have to be derived
             -- class , position where derived, type constructor
          -> [ImpDecl TokenId]  -- passes: import declarations of module
          -> Decls Id           -- updates: declarations of module
          -> IO ()

nhcDerive flags modidl  mrps  expFun userDefault tidFun tidFunSafe 
          intState derived impdecls decls {- constrs -} =
  {-profile "derive" $-}
  case (derive tidFun intState derived decls) of
    Left errors -> do
      pF (True) "Deriving failed:" (mixLine errors) 
      exit
    Right (intState',decls') -> do
      pF (sDerive flags) "Declarations after deriving:" 
          (prettyPrintId flags intState' ppTopDecls decls') 
      pF (sDBound flags) "Symbol table after deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable intState')))) 
      nhcDbgDataTrans flags modidl mrps expFun userDefault tidFun 
        tidFunSafe intState' impdecls decls'


{-
Debugging source-to-source translation of data type definitions.
For tracing only.
Transforms data type definitions and all type expressions occurring in
declarations.
-} 
nhcDbgDataTrans :: Flags           -- reads: compiler flags
                -> PackedString    -- passes: module identifier
                -> a 
                -> b 
                -> Maybe [Id]      -- passes: user defaults for Num classes
                -> ((TokenId,IdKind) -> Id) 
                   -- reads: maping from id token and kind to internal id
                -> c 
                -> IntState       -- updates: internal compiler state
                -> [ImpDecl TokenId] -- passes: import declarations of module
                -> Decls Id       -- updates: declarations of program
                -> IO ()  

nhcDbgDataTrans flags modidl mrps expFun userDefault tidFun tidFunSafe 
                intState {- importState derived -} impdecls decls =
 do
  let (decls'{-, derived'-}, intState', constrs) = 
        dbgDataTrans flags intState (error "repTree") tidFun {-derived-} decls 
  pF (sTraceData flags) "Abstract syntax tree after tracing type transformation"
     (prettyPrintId flags intState' ppTopDecls decls') 

  nhcExtract flags modidl mrps  expFun userDefault tidFun tidFunSafe 
    decls' constrs impdecls intState'


{-
Adds arity of all defined variables to symbol table of internal state.
Adds type of variables from type declarations and primitive and foreign
function definitions to symbol table of internal state
(but not type declarations from classes).
May discover a few errors and add appropriate messages to internal state.
-}
nhcExtract :: Flags           -- passes: compiler flags
           -> PackedString    -- passes: module identifier
           -> a 
           -> b 
           -> Maybe [Id]      -- passes: user defaults for Num classes
           -> ((TokenId,IdKind) -> Id)  
              -- passes: maping from id token and kind to internal id
           -> c 
           -> Decls Id       -- reads: declarations of program
           -> Maybe [(Pos,Id)] 
              -- passes between 2 tracing transformations
              -- data constructors defined in the module (by data/newtype)
           -> [ImpDecl TokenId] -- passes: import declarations of module
           -> IntState        -- updates: internal compiler state
           -> IO ()

nhcExtract flags modidl mrps expFun userDefault tidFun 
           tidFunSafe decls constrs impdecls intState = do
  nhcDbgTrans flags modidl mrps expFun userDefault tidFun 
    tidFunSafe decls constrs impdecls (extract decls intState)


{-
Debugging source-to-source translation of function definitions 
(for tracing only)
Reads the types put into the symbol table of internal state by extract pass.
-}
nhcDbgTrans :: Flags             -- reads: compiler flags
            -> PackedString      -- reads: module identifier
            -> a 
            -> b 
            -> Maybe [Id]        -- passes: user defaults for Num classes
            -> ((TokenId,IdKind) -> Id) 
            -- reads: maping from id token and kind to internal id
            -> c 
            -> Decls Id          -- updates: declarations of program
            -> Maybe [(Pos,Id)] 
               -- uses: data constructors defined in module (by data/newtype)
            -> [ImpDecl TokenId] -- reads: import declarations of module
            -> IntState          -- updates: internal compiler state
            -> IO () 

nhcDbgTrans flags modidl mrps expFun userDefault tidFun 
            tidFunSafe decls constrs impdecls state =
 -- profile "dbgtrans" $
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
        pF (sEBound flags) "Symbol table after extract:"  
           (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
        pF (True) "Error after extract:" (mixLine errors) 
        exit


{-
Create selectors for record fields.
(replace DeclConstrs by definitions for the selectors)
-} 
nhcRemove :: Flags 
          -> PackedString 
          -> a 
          -> b 
          -> Maybe [Id] 
          -> ((TokenId,IdKind) -> Id) 
          -> c 
          -> (Decls Id
             ,IntState
             ,SRIDTable) -- passes: table for source refs and ids for tracing
          -> IO () 

nhcRemove flags modidl  mrps expFun userDefault tidFun tidFunSafe 
          (decls, state, sridt) =
  {-profile "remove" $-} do
  pF (sTraceFns flags) "Tracing Transformation on function definitions"
                       (prettyPrintId flags state ppTopDecls decls) 
  nhcScc flags modidl mrps expFun userDefault tidFun tidFunSafe sridt 
    (removeDecls decls tidFun state)


{- 
First replace class and instance declarations by their type and method 
declarations; also fix arity of method definitions.
Strongly Connected Component analysis 
-}
nhcScc :: Flags 
       -> PackedString 
       -> a 
       -> b 
       -> Maybe [Id] 
       -> ((TokenId,IdKind) -> Id) 
       -> c 
       -> SRIDTable -- passes: table for source refs and ids for tracing
       -> (Decls Id,[Int],IntState) 
       -> IO ()

nhcScc flags modidl  mrps expFun userDefault tidFun tidFunSafe sridt 
       (decls,zcon,state) =
  {-profile "scc" $-}
  case getErrors state of
    (state,[]) -> do
      pF (sRemove flags) "Declarations after remove fields:" 
         (prettyPrintId flags state ppTopDecls decls) 
      case rmClasses tidFun state decls of
	(code,decls,state) ->
	  nhcType flags zcon modidl  mrps  expFun userDefault tidFun 
            tidFunSafe state code sridt (sccTopDecls decls)
    (state,errors) -> do
      pF (True) "Error after remove fileds:" (mixLine errors)
      exit


{- 
Type inference.
Also remove do notation and record expressions.
-}
nhcType :: Flags 
        -> [Int] 
        -> PackedString 
        -> a 
        -> b 
        -> Maybe [Id]  -- types chosen by user for defaulting of Num classes
        -> ((TokenId,IdKind) -> Int) 
        -> c 
        -> IntState 
        -> [ClassCode (Exp Int) Int] 
        -> SRIDTable -- passes: table for source refs and ids for tracing
        -> Decls Id
        -> IO ()

nhcType flags zcon modidl  mrps  expFun userDefault tidFun tidFunSafe 
        intState code sridt decls =
  profile "type" $ do
  pF (sScc flags) "Declarations after scc:" 
    (prettyPrintId flags intState ppTopDecls decls)
  pF (sScc flags) "Class/instances after scc:" 
    (prettyPrintId flags intState ppClassCodes code)
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
             -> SRIDTable -- passes: table for source refs and ids for tracing 
             -> ([ClassCode (Exp Int) Int],Decls Int,IntState) 
             -> IO ()

nhcInterface flags zcon modidl mrps expFun tidFun tidFunSafe sridt 
             (code,decls,state) =
  let mod = reverse (unpackPS modidl) in
--profile "interface" $
  case getErrors state of
    (state,[]) -> do
      pF (sType flags) "Declarations after type deriving:" 
         (prettyPrintId flags state ppTopDecls decls) 
      pF (sTBound flags) "Symbol table after type deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
      pF (sRImport flags) ("Actual imports used by this module ("++mod++"):") 
         (mixLine (reportFnImports mod state)) 
      state <- if mod == "Main" 
                 then typeOfMain flags tidFun decls state 
                 else return state
      nhcWriteI flags (buildInterface flags modidl state) 
      nhcFixSyntax flags zcon tidFun code sridt
                           (fixSyntax (sDbgTrans flags) decls state tidFun)
    (state,errors) -> do
      pF (True) "Error after type deriving/checking" (mixLine errors)
      pF (sType flags) "Declarations after type deriving:" 
         (prettyPrintId flags state ppTopDecls decls) 
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
Also removes data constructors defined by newtype.
(actually done by preceding function)
-}
nhcFixSyntax :: Flags 
             -> [Int] 
             -> ((TokenId,IdKind) -> Id) 
             -> [ClassCode (Exp Int) Int] 
             -> SRIDTable -- passes: table for source refs and ids for tracing
             -> ([Decl Id],IntState,Tree (TokenId,Int)) 
             -> IO ()

nhcFixSyntax flags zcon tidFun code sridt (decls,state,t2i) =
  {-profile "fixsyntax" $-}  do
  pF (sFixSyntax flags) "Declarations after fixSyntax"
     (prettyPrintId flags state ppTopDecls (DeclsParse decls))
  pF (sFSBound flags) "Symbol table after fixSyntax:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state))))
  nhcCase flags zcon tidFun sridt 
    (caseTopLevel (if sPrelude flags
                     then "Prelude:"++ sSourceFile flags
                     else reverse (unpackPS (mrpsIS state)))
                  t2i code decls state tidFun)


{-
Remove pattern matching: Change all pattern matches to case expressions.
Go from Haskell syntax to STG language (PosLambda).
(actually done by preceding function)
-}
nhcCase :: Flags 
        -> [Int] 
        -> ((TokenId,IdKind) -> Id) 
        -> SRIDTable -- passes: table for source refs and ids for tracing
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcCase flags zcon tidFun sridt (decls,state) =
--profile "case" $
  case getErrors state of
    (state,errors) -> do
      pF (not (null errors)) "Warning pattern removal" (mixLine errors) 
      pF (sCase flags) "Declarations after case:"  
         (strPCode (strISInt state) decls) 
      pF (sCBound flags) "Symbol table after case:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state))))
      nhcPrim flags tidFun zcon sridt 
        (primCode primFlags (not (sDbgTrans flags)) tidFun state decls)


{-
Expand primitives ?
(actually done by preceding function)
-}
nhcPrim :: Flags 
        -> ((TokenId,IdKind) -> Id) 
        -> [Int] 
        -> SRIDTable -- passes: table for source refs and ids for tracing
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcPrim flags tidFun zcon sridt (decls,state) =
  {-profile "prim" $-} do
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
        -> ((TokenId,IdKind) -> Id) 
        -> [Int] 
        -> SRIDTable -- passes: table for source refs and ids for tracing
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcFree flags tidFun zcon sridt (decls,state) =
  {-profile "free" $-} do
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
          -> SRIDTable -- passes: table for source refs and ids for tracing
          -> ([(Int,PosLambda)],IntState) 
          -> IO ()

nhcCode1a flags tidFun zcon sridt (decls,state) = do
  pF (sArity flags) "Declarations after first arity grouping" 
     (strPCode (strISInt state) decls) 
  nhcLift flags tidFun zcon sridt (liftCode decls state tidFun)

{-
Lambda lift
introduces thunks
(actually done by preceding function)
-}
nhcLift :: Flags 
        -> a 
        -> [Int] 
        -> SRIDTable -- passes: table for source refs and ids for tracing 
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcLift flags tidFun zcon sridt (decls,state) =
  {-profile "lift" $-} do
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
          -> SRIDTable -- passes: table for source refs and ids for tracing
          -> ([(Int,PosLambda)],IntState) 
          -> IO ()

nhcCode1b flags tidFun zcon sridt (decls,state) = do
  pF (sArity flags) "Declarations after second arity grouping" 
     (strPCode (strISInt state) decls) 
  nhcAtom flags zcon sridt (posAtom state decls)


{-
Pos Atom (not sure what this does!)
(actually done by preceding function)
-}
nhcAtom :: Flags 
        -> [Int] 
        -> SRIDTable -- passes: table for source refs and ids for tracing
        -> ([(Int,PosLambda)],IntState) 
        -> IO ()

nhcAtom flags zcon sridt (decls,state) =
  {-profile "atom" $-} do
  pF (sAtom flags) "Declarations after atom:" (strPCode (strISInt state) decls)
  pF (sABound flags) "Symbol table after atom:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state))))
  dumpZCon flags state (gcodeZCon (sProfile flags) state zcon) sridt decls


{-
Dump zero-arity constructors to object file (as Gcode)
Also dump source references and ids table for tracing to object file.
(actually done by preceding function)
-}
dumpZCon :: Flags 
         -> IntState 
         -> [[Gcode]] 
         -> SRIDTable -- table for source refs and ids for tracing
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
  (eslabs,escode) <-
          if (sAnsiC flags) 
          then do
             let eslabs = dbgDumpSRIDTableC Labels handle state flags sridt 
                              (startEmitState Labels)
                 escode = dbgDumpSRIDTableC Code   handle state flags sridt 
                              (startEmitState Code)
             return (foldr (\a b-> gcodeGather Labels state b a) eslabs zcons
                    ,foldr (\a b-> gcodeGather Code   state b a) escode zcons)
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
            return (startEmitState Labels, startEmitState Code)
  profile "dump code" $
    dumpCode handle flags [] (gcodeFixInit state flags) eslabs escode decls


{-
Generate Gcode for functions: for each declaration, do
       STGGcode
        GcodeFix
        GcodeOpt1
        GcodeMem
        GcodeOpt2
        GcodeRel
-} 
dumpCode :: Handle 
         -> Flags 
         -> [Foreign] 
         -> (IntState,(Tree ((Int,Int),Int),(Tree ([Char],Int),[(Int,Gcode)])))
         -> EmitState 
         -> EmitState 
         -> [(Int,PosLambda)] 
         -> IO ()

dumpCode handle flags foreigns (state,fixState) eslabs escode [] =
     dumpCodeEnd handle flags state eslabs escode foreigns
             (gcodeFixFinish state fixState)
dumpCode handle flags foreigns (state,fixState) eslabs escode (decl:decls) =
  -- profile "dump code" $
     nhcCode2 handle flags fixState decls eslabs escode foreigns 
             (stgGcode (sProfile flags) state decl)

 
nhcCode2 handle flags fixState decls eslabs escode foreigns
                                             (gcode,state,newforeigns) =
  do
    pF (sGcode flags) "G Code" (concatMap (strGcode state) ( gcode)) 
    nhcCode3 handle flags decls eslabs escode (foreigns++newforeigns) 
        (gcodeFix flags state fixState gcode)


nhcCode3 handle flags decls eslabs escode foreigns (state,fixState,gcode) = do
  pF (sGcodeFix flags) "G Code (fixed)" (concatMap (strGcode state) ( gcode))
  nhcCode35 handle flags fixState decls eslabs escode foreigns
        (gcodeOpt1 state gcode)
 

nhcCode35 handle flags fixState decls eslabs escode foreigns (gcode,state) = do
  pF (sGcodeOpt1 flags) "G Code (opt1)" (concatMap (strGcode state) ( gcode))
  nhcCode4 handle flags fixState decls eslabs escode foreigns 
        (gcodeMem (sProfile flags) state gcode)

 
nhcCode4 handle flags fixState decls eslabs escode foreigns (gcode,state) = do
  pF (sGcodeMem flags) "G Code (mem)" (concatMap (strGcode state) ( gcode))
  nhcCode5 handle flags fixState decls eslabs escode foreigns
        (gcodeOpt2 state gcode)
 

nhcCode5 handle flags fixState decls eslabs escode foreigns (gcode,state) = do
  pF (sGcodeOpt2 flags) "G Code (opt2)" (concatMap (strGcode state) ( gcode))
  nhcCode6 handle flags fixState decls state eslabs escode foreigns
        (gcodeRel gcode)
 

nhcCode6 handle flags fixState decls state eslabs escode foreigns gcode = do
  pF (sGcodeRel flags) "G Code (rel)" (concatMap (strGcodeRel state) gcode)
  (eslabs',escode') <-
           if (sAnsiC flags) 
           then return (gcodeGather Labels state eslabs gcode
                       ,gcodeGather Code   state escode gcode)
           else do 
                  catch (hPutStr handle (foldr (gcodeDump state) "\n" gcode))
                        (\ioerror -> do
                                       hPutStr stderr 
                                         ("Failed appending to object file "
                                          ++ sObjectFile flags ++ ":"  
                                          ++ show ioerror ++ "\n") 
                                       exit) 
                  return (eslabs,escode)
  dumpCode handle flags foreigns (state,fixState) eslabs' escode' decls


{-
Dump Gcode to object file (as bytecode)
-}
dumpCodeEnd :: Handle 
            -> Flags 
            -> IntState 
            -> EmitState 
            -> EmitState 
            -> [Foreign] 
            -> [[Gcode]] 
            -> IO ()

dumpCodeEnd handle flags state eslabs escode foreigns gcode =
  {-profile "dump tables" $-} do
  pF (sGcodeRel flags) "G Code (rel)" 
     (concatMap (strGcodeRel state) (concat gcode)) 
  if (sAnsiC flags) 
    then do
       let eslabs' = foldr (\a b-> gcodeGather Labels state b a) eslabs gcode
           escode' = foldr (\a b-> gcodeGather Code   state b a) escode gcode
       catch (do hPutStr handle (gcodeCHeader)
                 hPutStr handle (emit Labels eslabs')
                 hPutStr handle (emit Code escode'))
             (\ioerror -> do hPutStr stderr 
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
           hPutStr handle "\n#include <haskell2c.h>\n#include <HsFFI.h>\n" 
           mapM_ (\f-> hPutStr handle (strForeign f "")) foreigns
  hClose handle


---   Small help functions

strISInt :: IntState -> Id -> String
strISInt state v = strIS state v ++ "{"++show v++"}"

{- End Module Main ----------------------------------------------------------}
