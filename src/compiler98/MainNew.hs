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
import ImportState(ImportState,Info,IE,initIS,getSymbolTableIS,getErrIS
                  ,getRenameTableIS)
import IntState(IntState,dummyIntState,getSymbolTable,getErrorsIS,strIS,mrpsIS)
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
            ,sEBound,sIIRename
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
import IExtract(getNeedIS,addPreludeTupleInstances)
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

import Foreign(Foreign,strForeign)
import ReportImports(reportFnImports)
import AuxFile(toAuxFile)
import AuxLabelAST(auxLabelSyntaxTree)
import TraceTrans(traceTrans)


--import NonStdProfile
--beginPhase str = profile str (return ())
beginPhase str = return ()

-- some miscellaneous settings
primFlags = (False   -- bool is not the same as Word
	    ,False   -- && || not is not primitive
	    ,False   -- translate into prim only when strict
	    )

-- some nicer error handling
catchError :: Either b a -> String -> (b->String) -> IO a
catchError comp errmsg showErrors = do
    case comp of
        Left errs -> do pF True errmsg (showErrors errs)
                        exit
        Right a   -> return a

-- for Hugs, which cannot read commandline args using System.getArgs:
gmain cml = main' (words cml)

-- for all other compilers:
main = do
  args <- getArgs
  main' args

main' args = do
  let flags = processArgs args
  let filename = sRealFile flags


  {- lex source code -}
  beginPhase "lex"
  mainChar	-- :: String
           <- catch (readFile filename) (can'tOpen filename) 
  lexdata	-- :: [PosToken]
           <- return (lexical (sUnderscore flags) (sSourceFile flags)
                              (if sUnlit flags 
                                then unlit (sSourceFile flags) mainChar 
                                else mainChar))
  pF (sLex flags) "Lexical" 
       (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata))


  {- parse source code -}
  beginPhase "parse"
  parsedPrg	-- :: Module TokenId
            <- catchError (parseit parseProg lexdata)
                          (sSourceFile flags) showErr
  pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedPrg) 


  {-
  -- Read and write auxiliary information files (for tracing).
  -- Then relabel the syntax tree with the auxiliary information.
  -- Eventually, the tracing transformation itself will also be in this
  -- phase of the compiler.
  -}
  if sHatAuxFile flags
    then do toAuxFile flags (sAuxFile flags) parsedPrg
          --putStrLn (prettyPrintTokenId flags ppModule parsedPrg)
            newprog <- auxLabelSyntaxTree flags parsedPrg
            putStrLn (prettyPrintTraceId flags ppModule newprog) 
            putStrLn "----------------------------------"    
            putStrLn (prettyPrintTokenId flags ppModule 
                       (traceTrans (sSourceFile flags) newprog))
            exitWith (ExitSuccess)
    else return ()


  {- 
  -- Perform "need" analysis (what imported entities are required?) 
  -- Second argument may contain error message or parse tree
  -- Creates ImportState for next pass.
  -}
  (Module _ (Visible modid) _ _ inf _) <- return parsedPrg
  -- Insert check that sPart flags or modid == sourcefile ???
  parsedPrg' <- return (
          dbgAddImport (sDbgTrans flags || sDbgPrelude flags) parsedPrg)

  beginPhase "need"
  (need		-- :: NeedTable
   ,qualFun	-- :: TokenId -> [TokenId]
   ,overlap	-- :: Overlap
   ,info)	-- :: Either String (expFun,imports)
         <- return (needProg flags parsedPrg' inf)
  (expFun	-- :: Bool -> Bool -> TokenId -> IdKind -> IE
   ,imports)	-- :: [ ( PackedString
		--      , (PackedString, PackedString, Tree (TokenId,IdKind))
		--            -> [[TokenId]] -> Bool
		--      , HideDeclIds
		--      )
		--    ]
            <- catchError info
                          (sSourceFile flags) id
  pF (sNeed flags) "Need (after reading source module)"  
            (show (treeMapList (:) need)) 


  {- Parse interface files for imported modules -}
  beginPhase "imports"
  importState	-- :: ImportState
              <- nhcImport flags (addPreludeTupleInstances () (initIS need))
                           imports

  {-
  -- Rename identifiers (also patches fixity information)
  -- Changes from ImportState to IntState
  -}
  (Module _ (Visible modid) _ impdecls inf decls) <- return parsedPrg'

  beginPhase "rename"
  (decls	-- :: Decls Id  (renamed from decls :: Decls TokenId)
   ,state	-- :: IntState  (internal compiler state)
   ,tidFun	-- :: ((TokenId,IdKind) -> Id) 
		--     (mapping from id token and kind to internal id)
   ,tidFunSafe	--   tidFunSafe does not appear to be used anywhere!
   ,derived	-- :: [(Id,[(Pos,Id)])] 
		--     instances that have to be derived:
		--     class, position where derived, type constructor
   ,userDefault	-- :: Maybe [Id]  (user defaults for Num classes)
   ,rt)		-- :: RenameTable
       <- catchError (rename flags modid qualFun expFun inf decls
                             importState overlap)
                     "Error when renaming:" mixLine
  pF (sRename flags) "Declarations after rename and fixity:" 
        (prettyPrintId flags state ppTopDecls decls) 
  pF (sRBound flags) "Symbol table after rename and fixity:"  
        (mixLine (map show (treeMapList (:) (getSymbolTable state))))
  catchError (getErrorsIS state) "Error after rename:" mixLine


  {- Record dependencies in .dep file -}
  depend flags state rt 


  {-
  -- For foreign imports, transform all functions with IO types to introduce
  -- the appropriate wrapper.
  -}
  beginPhase "ffitrans"
  (decls	-- :: Decls Id
   ,state)	-- :: IntState
             <- return (ffiTrans decls tidFun state)


  {- Derive class instances where required by data definitions -}
  beginPhase "derive"
  (state	-- :: IntState
   ,decls)	-- :: Decls Id
           <- catchError (derive tidFun state derived decls)
                         "Deriving failed:" mixLine
  pF (sDerive flags) "Declarations after deriving:" 
          (prettyPrintId flags state ppTopDecls decls) 
  pF (sDBound flags) "Symbol table after deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 


  {-
  -- Debugging source-to-source translation of data type definitions.
  -- For tracing only.
  -- Transforms data type definitions and all type expressions occurring in
  -- declarations.
  -} 
  beginPhase "dbgdatatrans"
  (decls	-- :: Decls Id
   ,state	-- :: IntState
   ,constrs)	-- :: Maybe [(Pos,Id)]
             <- return (dbgDataTrans flags state (error "repTree")
                                     tidFun decls)
  pF (sTraceData flags) "Abstract syntax tree after tracing type transformation"
     (prettyPrintId flags state ppTopDecls decls) 


  {-
  -- Adds arity of all defined variables to symbol table of internal state.
  -- Adds type of variables from type declarations and primitive and foreign
  -- function definitions to symbol table of internal state
  -- (but not type declarations from classes).
  -- May discover a few errors and add appropriate messages to internal state.
  -}
  beginPhase "extract"
  state	-- :: IntState
           <- return (extract decls state)
  pF (sEBound flags) "Symbol table after extract:"  
           (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
  catchError (getErrorsIS state) "Error after extract:" mixLine


  {-
  -- Debugging source-to-source translation of function definitions 
  -- (for tracing only)
  -- Reads the types put into the symbol table of internal state by
  -- the extract pass.
  -- sridt :: SRIDTable   (table for source refs and ids for tracing)
  -}
  beginPhase "dbgtrans"
  (decls	-- :: Decls Id
   ,state	-- :: IntState
   ,sridt)	-- :: SRIDTable
          <- return (if sDbgTrans flags 
                       then debugTrans flags state tidFun modid
                                       (sSourceFile flags) impdecls
                                       decls constrs 
                       else (decls, state, Nothing))
  pF (sTraceFns flags) "Tracing Transformation on function definitions"
                       (prettyPrintId flags state ppTopDecls decls) 


  {-
  -- Create selectors for record fields.
  -- (replace DeclConstrs by definitions for the selectors)
  -} 
  beginPhase "remove fields"
  (decls	-- :: Decls Id
   ,zcon	-- :: [Int]
   ,state)	-- :: IntState
          <- return (removeDecls decls tidFun state)
  pF (sRemove flags) "Declarations after remove fields:" 
         (prettyPrintId flags state ppTopDecls decls) 
  catchError (getErrorsIS state) "Error after remove fields:" mixLine


  {- 
  -- First replace class and instance declarations by their type and method 
  -- declarations; also fix arity of method definitions.
  -- Strongly Connected Component analysis 
  -}
  beginPhase "scc"
  (code		-- :: [ClassCode (Exp Id) Id]
   ,decls	-- :: Decls Id
   ,state)	-- :: IntState
        <- return (rmClasses tidFun state decls)
  decls		-- :: Decls Id
        <- return (sccTopDecls decls)
  pF (sScc flags) "Declarations after scc:" 
    (prettyPrintId flags state ppTopDecls decls)
  pF (sScc flags) "Class/instances after scc:" 
    (prettyPrintId flags state ppClassCodes code)


  {- 
  -- Type inference.
  -- Also remove do notation and record expressions.
  -}
  beginPhase "type"
  (code		-- :: [ClassCode (Exp Id) Id]
   ,decls	-- :: Decls Id
   ,state)	-- :: IntState
          <- return (typeTopDecls tidFun userDefault state
                                  code (sDbgTrans flags) decls)
  pF (sType flags) "Declarations after type deriving:" 
         (prettyPrintId flags state ppTopDecls decls) 
  pF (sTBound flags) "Symbol table after type deriving:"  
         (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 
  catchError (getErrorsIS state) "Error after type deriving/checking" mixLine


  {- Build interface file for this module and write it out -}
  beginPhase "interface"
  let modname = reverse (unpackPS modid)
  pF (sRImport flags) ("Actual imports used by this module ("++modname++"):") 
         (mixLine (reportFnImports modname state)) 
  state <- if modname == "Main" 
                 then typeOfMain flags tidFun decls state 
                 else return state
  catch (writeFile (sTypeFile flags) (buildInterface flags modid state))
        (\ioerror -> do
                       hPutStr stderr ("Couldn't write interface file "
                                       ++ sTypeFile flags ++ ":" 
                                       ++ show ioerror ++ "\n") 
                       exit)

  {- 
  -- Fix syntax (small tweaks based on type information) 
  -- optimisation: evaluation of `fromInteger' where possible
  -- Also removes data constructors defined by newtype.
  -}
  beginPhase "fixsyntax"
  (decls	-- :: [Decl Id]
   ,state	-- :: IntState
   ,t2i)	-- :: Tree (TokenId,Id)
        <- return (fixSyntax (sDbgTrans flags) decls state tidFun)
  pF (sFixSyntax flags) "Declarations after fixSyntax"
          (prettyPrintId flags state ppTopDecls (DeclsParse decls))
  pF (sFSBound flags) "Symbol table after fixSyntax:"  
          (mixLine (map show (treeMapList (:) (getSymbolTable state))))


  {-
  -- Remove pattern matching: Change all pattern matches to case expressions.
  -- Go from Haskell syntax to STG language (PosLambda).
  -}
  beginPhase "case"
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (caseTopLevel (if sPrelude flags
                                   then "Prelude:"++ sSourceFile flags
                                   else reverse (unpackPS (mrpsIS state)))
                                   t2i code decls state tidFun)
  case getErrorsIS state of
    Left errors ->
      pF True "Warning pattern removal" (mixLine errors) 
    _ -> return ()
  pF (sCase flags) "Declarations after case:"  
          (strPCode (strISInt state) decls) 
  pF (sCBound flags) "Symbol table after case:"  
          (mixLine (map show (treeMapList (:) (getSymbolTable state))))


  {- Expand primitives -}
  beginPhase "prim"
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (primCode primFlags (not (sDbgTrans flags))
                              tidFun state decls)
  pF (sPrim flags) "Declarations after prim expand:" 
          (strPCode (strISInt state) decls) 
  pF (sPBound flags) "Symbol table after prim expand:"  
          (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 


  {- Determine free variables (for lambda lifting) -}
  beginPhase "free"
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (freeVar (sKeepCase flags) decls state)
  pF (sFree flags) "Declarations with explicit free variables:" 
     (strPCode (strISInt state) decls)


  {- Do arity grouping on declarations (for lambda lifting) -}
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (stgArity state decls)
  pF (sArity flags) "Declarations after first arity grouping" 
     (strPCode (strISInt state) decls) 


  {- Lambda lift, introduces thunks -}
  beginPhase "lift"
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (liftCode decls state tidFun)
  pF (sLift flags) "Declarations after lambda lifting:" 
     (strPCode (strISInt state) decls) 
  pF (sLBound flags) "Symbol table after lambda lifting:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state)))) 

    
  {- Do arity grouping again -}
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (stgArity state decls)
  pF (sArity flags) "Declarations after second arity grouping" 
     (strPCode (strISInt state) decls) 


  {- Pos Atom (not sure what this does!) -}
  beginPhase "atom"
  (decls	-- :: [(Id,PosLambda)]
   ,state)	-- :: IntState
          <- return (posAtom state decls)
  pF (sAtom flags) "Declarations after atom:" (strPCode (strISInt state) decls)
  pF (sABound flags) "Symbol table after atom:"  
     (mixLine (map show (treeMapList (:) (getSymbolTable state))))


  {-
  -- Dump zero-arity constructors to object file (as Gcode)
  -- Also dump source references and ids table for tracing to object file.
  -}
  beginPhase "dump zcon"
  zcons		-- :: [[Gcode]]
        <- return (gcodeZCon (sProfile flags) state zcon)
  handle <- catch (openFile (sObjectFile flags) WriteMode) 
	          (\ioerror -> do
                                 hPutStr stderr ("Couldn't open object file "
                                   ++ sObjectFile flags ++ ":" 
                                   ++ show ioerror ++ "\n")  
                                 exit) 
  (eslabs	-- :: EmitState
   ,escode)	-- :: EmitState
       <- if (sAnsiC flags) 
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


  {- Generate Gcode for functions -} 
  beginPhase "generate code"
  (state	-- :: IntState
   ,fixState)	-- :: (Tree ((Id,Id),Id), (Tree (String,Id), [(Id,Gcode)]))
             <- return (gcodeFixInit state flags)

  (state	-- :: IntState
   ,fixState	-- :: (Tree ((Id,Id),Id), (Tree (String,Id), [(Id,Gcode)]))
   ,foreigns	-- :: [Foreign]
   ,eslabs	-- :: EmitState
   ,escode)	-- :: EmitState
           <- generateCode handle flags [] state fixState eslabs escode decls

  gcode		-- :: [[Gcode]]
        <- return (gcodeFixFinish state fixState)
  pF (sGcodeRel flags) "G Code (rel)" 
     (concatMap (strGcodeRel state) (concat gcode)) 


  {- Dump Gcode to object file (as bytecode) -}
  beginPhase "write code"
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


--------


type FixState = (Tree ((Id,Id),Id), (Tree (String,Id), [(Id,Gcode)]))


{-
-- Generate Gcode for functions: for each declaration, do
--        STGGcode
--         GcodeFix
--         GcodeOpt1
--         GcodeMem
--         GcodeOpt2
--         GcodeRel
-} 
generateCode :: Handle 
                -> Flags 
                -> [Foreign] 
                -> IntState
                -> FixState
                -> EmitState 
                -> EmitState 
                -> [(Int,PosLambda)] 
                -> IO (IntState,FixState,[Foreign],EmitState,EmitState)

generateCode handle flags foreigns state fixState eslabs escode [] =
  return (state, fixState, foreigns, eslabs, escode)

generateCode handle flags foreigns state fixState eslabs escode (decl:decls)
  = do
  (gcode	-- :: [Gcode]
   ,state	-- :: IntState
   ,newforeigns)-- :: [Foreign]
          <- return (stgGcode (sProfile flags) state decl)
  pF (sGcode flags) "G Code" (concatMap (strGcode state) gcode) 

  (state	-- :: IntState
   ,fixState	-- :: FixState
   ,gcode)	-- :: [Gcode]
          <- return (gcodeFix flags state fixState gcode)
  pF (sGcodeFix flags) "G Code (fixed)" (concatMap (strGcode state) gcode)

  (gcode	-- :: [Gcode]
   ,state)	-- :: IntState
          <- return (gcodeOpt1 state gcode)
  pF (sGcodeOpt1 flags) "G Code (opt1)" (concatMap (strGcode state) gcode)

  (gcode	-- :: [Gcode]
   ,state)	-- :: IntState
          <- return (gcodeMem (sProfile flags) state gcode)
  pF (sGcodeMem flags) "G Code (mem)" (concatMap (strGcode state) gcode)

  (gcode	-- :: [Gcode]
   ,state)	-- :: IntState
          <- return (gcodeOpt2 state gcode)
  pF (sGcodeOpt2 flags) "G Code (opt2)" (concatMap (strGcode state) gcode)

  gcode		-- :: [Gcode]
          <- return (gcodeRel gcode)
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
  generateCode handle flags (foreigns++newforeigns) state fixState
               eslabs' escode' decls



--------

{- Parse interface files for imported modules -}
nhcImport :: Flags 
          -> ImportState 
          -> [(PackedString
              ,   (PackedString,PackedString,Tree (TokenId,IdKind)) 
               -> [[TokenId]] 
               -> Bool
              ,HideDeclIds
              )] 
          -> IO ImportState

nhcImport flags importState [] = do
  --beginPhase "import []"
  pF (sINeed flags) "Need after all imports"    
             (show (treeMapList (:)  (thd3 (getNeedIS importState)))) 
  pF (sIBound flags) "Symbol table after import"  
             (mixLine (map show (treeMapList 
                                   (:) (getSymbolTableIS importState)))) 
  pF (sIRename flags) "Rename table after import"  
             (mixLine (map show (treeMapList 
                                   (:) (getRenameTableIS importState)))) 
  catchError (getErrIS importState) "Error after import " mixLine
  return importState

nhcImport flags importState (x:xs) = do
  --trace ("import:" ++ (reverse . show . fst3) x) $
  --beginPhase ("import:" ++ (reverse . show . fst3) x)
  let fname = (reverse . unpackPS . (\(y,_,_)->y)) x
  importState <- importOne flags importState x 
  pF (sIINeed flags) ("Intermediate need after import "++fname)
       (show (treeMapList (:)  (thd3 (getNeedIS importState))))
  pF (sIIBound flags) ("Intermediate symbol table after import "++fname)
       (mixLine (map show (treeMapList (:) (getSymbolTableIS importState))))
  pF (sIIRename flags) ("Intermediate rename table after import "++fname)
       (mixLine (map show (treeMapList (:) (getRenameTableIS importState)))) 
  nhcImport flags importState xs
    

---   Small help functions

strISInt :: IntState -> Id -> String
strISInt state v = strIS state v ++ "{"++show v++"}"

{- End Module Main ----------------------------------------------------------}
