{- ---------------------------------------------------------------------------
Flags are all the choices and information given to the compiler in the 
argument list. Here a data type Flags is defined for holding this information,
a function processArgs to obtain a value of type Flags from the argument list,
and a simple function pF for printing information demanded by a flag.
-}
module Flags where
{- export list does not work with current nhc
            (Flags,processArgs,pF
            ,sProfile,sRedefine,sUnix,sUnlit,sSourceFile,sUnderscore,sLex
            ,sDbgPrelude,sDbgTrans,sNeed,sParse,sIRename,sIBound,sINeed
            ,sIIBound,sIINeed,sRBound,sRename,sDbg,sDBound,sDerive,sEBound
            ,sDbg2,sRemove,sScc,sRImport,sTBound,sType,sTypeFile,sPrelude
            ,sFSBound,sFixSyntax,sCBound,sCase,sKeepCase,sPBound,sPrim,sFree
            ,sArity,sLBound,sLift,sProfile,sABound,sAtom,sAnsiC,sObjectFile
            ,sGcode,sGcodeFix,sGcodeOpt1,sGcodeMem,sGcodeOpt2,sGcodeRel
            ,sNplusK,sPuns,sPreludes,sIncludes,sImport,sILex,sPart,sLib
            ,sDbgTrusted,sTprof,sFunNames,sDepend,sRealFile) where
-}

import IO
import OsOnly(fixRootDir,fixTypeFile,fixObjectFile)

data Flags = FF 
  {sRealFile :: String
  ,sSourceFile :: String
  ,sTypeFile :: String
  ,sObjectFile :: String
  ,sIncludes :: [String]
  ,sPreludes :: [String]
--v Flags to control compilation
  ,sRedefine :: Bool
  ,sPart :: Bool
  ,sUnix :: Bool
  ,sUnlit :: Bool

  ,nProfile :: Int
  ,sZap :: Bool
  ,sPrelude :: Bool
--v Flags for machine architecture
  ,sAnsiC :: Bool
  ,s64bit :: Bool
  ,sTprof :: Bool
  ,sNplusK :: Bool
  ,sUnderscore :: Bool
  ,sPuns :: Bool
--v debugging flags
  ,sLex :: Bool
  ,sParse :: Bool
  ,sNeed :: Bool
  ,sINeed :: Bool
  ,sIRename :: Bool
  ,sIBound :: Bool
  ,sLib :: Bool      -- not a debugging flag, but it was free
  ,sIINeed :: Bool
  ,sIIBound :: Bool
  ,sRename :: Bool
  ,sRBound :: Bool
  ,sDerive :: Bool
  ,sDBound :: Bool
  ,sPBound :: Bool
  ,sEBound :: Bool
  ,sRemove :: Bool
  ,sScc :: Bool
  ,sType :: Bool
  ,sTBound :: Bool
  ,sFixSyntax :: Bool
  ,sSTG :: Bool
  ,sFSBound :: Bool
  ,sLift :: Bool
  ,sLBound :: Bool
  ,sCase :: Bool
  ,sCBound :: Bool
  ,sPrim :: Bool
  ,sBCBefore :: Bool

  ,sGcode :: Bool
  ,sGcodeFix :: Bool
  ,sGcodeMem :: Bool
  ,sGcodeOpt1 :: Bool
  ,sGcodeRel :: Bool
  ,sKeepCase :: Bool
  ,sArity :: Bool

  ,sSTGCode :: Bool
  ,sGcodeOpt2 :: Bool
  ,sImport :: Bool
  ,sDepend :: Bool
  ,sFree :: Bool
  ,sAtom :: Bool
  ,sABound :: Bool
  
  ,sFunNames :: Bool
  ,sILex :: Bool
  ,sIParse :: Bool
  ,sRImport :: Bool
  
  ,sDbgTrans :: Bool
  ,sDbg :: Bool
  ,sDbg2 :: Bool
  ,sDbgPrelude :: Bool
  ,sDbgTrusted :: Bool
  }
  deriving Show


-- not a selector, but a function:
sProfile :: Flags -> Bool
sProfile flags = nProfile flags > (0::Int)

  

{- If first argument is True, then print second and third with formatation -}

pF :: Bool -> [Char] -> [Char] -> IO ()

pF flag title text =
  if flag 
    then hPutStr stderr ( "\n====================================\n\t" 
                          ++ title++":\n"++text++"\n") 
    else return ()

{- ---------------------------------------------------------------------------
All the following functions obtain information from the argument list of the
compiler to set flags appropriately.
-}

{-
The main function for processing the argument list.
Aborts with error, if the required filenames are not in argument list.
(But no further error checking)
-}

processArgs :: [String] -> Flags

processArgs xs = flags
 where
 (rootdir,filename) = fixRootDir isUnix sourcefile
 isUnix = sUnix flags
 (realfile,sourcefile,typefile,cfile) =
   case getFiles xs of
     [sourcefile] -> (sourcefile
                     ,sourcefile
                     ,fixTypeFile isUnix rootdir filename
                     ,fixObjectFile isUnix rootdir filename)
     [sourcefile,typefile,cfile] -> (sourcefile,sourcefile,typefile,cfile)
     [realfile,sourcefile,typefile,cfile] -> (realfile,sourcefile
                                             ,typefile,cfile)
     _ -> error ("\nusage: nhc98comp file.hs\n       nhc98comp sourcefile interfacefile C-file\n       nhc98comp sourcefile sourcename interface C-file\n")  
 flags = FF
  {sRealFile=realfile
  ,sSourceFile=sourcefile
  ,sTypeFile=typefile
  ,sObjectFile=cfile
  ,sIncludes=rootdir:getIncludes xs
  ,sPreludes=getPreludes xs
  ,sRedefine = fElem False "redefine" xs 
  -- ^ Don't complain if redefining an imported identifier
  ,sPart = fElem False "part" xs      	        
  -- ^ Compiling part of a lib, so don't complain if module name differs 
  -- from file name and don't create 
  -- profiling information for this module
  ,sUnix = fElem True  "unix" xs          	
  -- ^ Use unix file names
  ,sUnlit = fElem False "unlit" xs         	
  -- ^ Unliterate the source code
  ,nProfile = length (filter (== "-profile") xs)	
  -- ^ amount of profiling information / node
  ,sZap = fElem True "zap" xs             	
  -- Generate code to zap unused arguments/stack positions
  ,sPrelude = fElem False "prelude" xs		
  -- Keep prelude definitions in interface file

  ,sAnsiC = fElem True  "ansiC" xs    -- Generate bytecode as ANSI C file
  ,s64bit = fElem False "64bit" xs    -- 32bit/64bit word size (ignored)
  ,sTprof = fElem False "tprof" xs    -- generate for time profiling PH
  ,sNplusK = fElem False "nkpat" xs   -- Enable (n+k) patterns
  ,sUnderscore = fElem False "underscore" xs 
  -- ^ Enable H'98 underscore-is-lower-case
  ,sPuns  = fElem True  "puns" xs      -- Enable pre-98 named-field puns
  ,sLex = fElem False "lex" xs         -- show lexical input
  ,sParse  = fElem False "parse" xs    -- show syntax tree  after  parser
  ,sNeed = fElem False "need" xs       -- show need   table before import
  ,sINeed = fElem False "ineed" xs     -- show need   table after  import
  ,sIRename = fElem False "irename" xs -- show rename table after  import
  ,sIBound = fElem False "ibound" xs   -- show symbol table after  import
  ,sLib = fElem False "lib" xs         
  -- ^ Compiling a lib, don't complain if importing modules with names 
  -- that differs from their filename.
  ,sIINeed = fElem False "iineed" xs   
  -- ^ show need   table between all import files
  ,sIIBound = fElem False "iibound" xs 
  -- ^ show symbol table between all import files
  ,sRename = fElem False "rename" xs   -- show syntax tree  after   rename
  ,sRBound = fElem False "rbound" xs   -- show symbol table after   rename
  ,sDerive = fElem False "derive" xs   -- show syntax tree  after   derive
  ,sDBound = fElem False "dbound" xs   -- show symbol table after   derive
  ,sPBound = fElem False "pbound" xs   

  -- ^ show symbol table after inserting primitive functions
  ,sEBound = fElem False "ebound" xs   -- show symbol table after extract
  ,sRemove = fElem False "remove" xs   
  -- ^ show syntax tree  after fields are removed (translated into selectors)
  ,sScc = fElem False "scc" xs               	
  -- ^ show syntax tree  after splitting into strongly connected groups
  ,sType = fElem False "type" xs              	
  -- ^ show syntax tree  after type check
  ,sTBound = fElem False "tbound" xs   -- show symbol table after type check
  ,sFixSyntax = fElem False "fixsyntax" xs           
  -- ^ show syntax tree  after removing newtype constructors and 
  -- fixing Class.Type.metod
  ,sSTG = fElem False "stg" xs	       
  -- ^ show stg    tree  after translation from syntax tree 
  ,sFSBound = fElem False "fsbound" xs            	
  -- ^ show symbol table after adding Class.Type.metod info
  ,sLift = fElem False "lift" xs              	
  -- ^ show syntax tree  after lambda lifting
  ,sLBound = fElem False "lbound" xs            	
  -- ^ show symbol table after lambda lifting
  ,sCase = fElem False "case" xs              	
  -- ^ show stg    tree  after simplification of patterns
  ,sCBound = fElem False "cbound" xs            	
  -- ^ show symbol table after simplification of pattern
  ,sPrim = fElem False "prim" xs              	
  -- ^ show stg    tree  after inserting primitive functions
  ,sBCBefore = fElem False "bcbefore" xs		
  -- ^ show stg    tree  before converting to byte code
  

  ,sGcode = fElem False "gcode" xs            	
  -- ^ show G code    	  -- NR
  ,sGcodeFix = fElem False "gcodefix" xs            
  -- ^ show G code after large constant fixed -- NR
  ,sGcodeMem = fElem False "gcodemem" xs     -- show G code NEEDHEAP
  ,sGcodeOpt1 = fElem False "gcodeopt1" xs   -- show G code optimisation
  ,sGcodeRel = fElem False "gcoderel" xs     -- show G code after offsets -- NR
  ,sKeepCase = fElem False "keepcase" xs     
  -- ^ Don't lift case, we fix those later
  ,sArity = fElem False "arity" xs           -- show stg    tree  after arity
  ,sSTGCode = fElem False "stgcode" xs       -- show STG code	-- DW
  ,sGcodeOpt2 = fElem False "gcodeopt2" xs   -- show G code optimisation
  ,sImport = fElem False "import" xs         -- print name of imported files
  ,sDepend = fElem False "depend" xs	     
  -- ^ print imported identifiers that are used (not even alpha yet)
  ,sFree = fElem False "free" xs	     
  -- ^ show stg    tree  with explicyr free variables
  ,sAtom = fElem False "atom" xs		
  -- ^ show stg    tree  after only atoms in applications
  ,sABound = fElem False "abound" xs		
  -- ^ show symbol table after only atoms in applications
  ,sFunNames = fElem False "funnames" xs            
  -- ^ insert position and name of functions in the code
  ,sILex = fElem False "ilex" xs             -- show lexical input
  ,sIParse = fElem False "iparse" xs         -- show syntax tree  after  parser
  ,sRImport = fElem False "report-imports" xs	
  -- ^ show only imports actually used
  ,sDbgTrans = fElem False "dbgtrans" xs     -- perform debugging translation
  ,sDbg = fElem False "dbg" xs	      	     
  -- ^ show ast after debugging translation
  ,sDbg2 = fElem False "dbg2" xs	     -- more debugging output
  ,sDbgPrelude = fElem False "dbgprelude" xs -- use the debugging prelude
  ,sDbgTrusted = fElem False "trusted" xs    
  -- ^ A "trusted" module (don't trace)
  }
  
  
{- obtain list of filenames from argument list -}
getFiles :: [String] -> [String]

getFiles = filter (\xs -> case xs of ('-':_) -> False ; _ -> True)


{- obtain list of include paths from argument list -}
getIncludes :: [String] -> [String]

getIncludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'i':_) -> True 
                                        ('-':'I':_) -> True  
                                        _           -> False)

{- obtain list of prelude paths from argument list -}
getPreludes :: [String] -> [String]

getPreludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'P':_) -> True ; _ -> False)


{-
Returns if given option is set or not in argument list.
If it is neither set nor unset, then default value (first arg.) is returned.
-}
fElem :: Bool -> [Char] -> [String] -> Bool

fElem def f flags = if ('-':f) `elem` flags then True
                    else if ('-':'n':'o':f) `elem` flags then False
                    else def


