{- ---------------------------------------------------------------------------
Flags are all the choices and information given to the compiler in the 
argument list. Here a data type Flags is defined for holding this information,
a function processArgs to obtain a value of type Flags from the argument list,
and a simple function pF for printing information demanded by a flag.
-}
module Flags
  (Flags
  ,processArgs
  ,pF
  ,sProfile
  ,sRedefine
  ,sUnix
  ,sUnlit
  ,sSourceFile
  ,sUnderscore
  ,sLex
  ,sDbgPrelude
  ,sDbgTrans
  ,sNeed
  ,sParse
  ,sIRename
  ,sIBound
  ,sINeed
  ,sIIBound
  ,sIINeed
  ,sIIRename
  ,sRBound
  ,sRename
  ,sTraceData
  ,sDBound
  ,sDerive
  ,sEBound
  ,sTraceFns
  ,sRemove
  ,sScc
  ,sRImport
  ,sTBound
  ,sType
  ,sTypeFile
  ,sPrelude
  ,sFSBound
  ,sFixSyntax
  ,sCBound
  ,sCase
  ,sKeepCase
  ,sPBound
  ,sPrim
  ,sFree
  ,sArity
  ,sLBound
  ,sLift
  ,sProfile
  ,sABound
  ,sAtom
  ,sAnsiC
  ,sObjectFile
  ,sGcode
  ,sGcodeFix
  ,sGcodeOpt1
  ,sGcodeMem
  ,sGcodeOpt2
  ,sGcodeRel
  ,sNplusK
  ,sPuns
  ,sPreludes
  ,sIncludes
  ,sImport
  ,sILex
  ,sPart
  ,sLib
  ,sDbgTrusted
  ,sTprof
  ,sFunNames
  ,sDepend
  ,sRealFile
  ,sShowType
  ,sShowWidth
  ,sShowIndent
  ,sShowQualified
  ,sHiSuffix
--  ,sHatTrans
--  ,sHatAuxFile
--  ,sHatTransFile
--  ,sHatFileBase
--  ,sHatTransFile
  ) where

import IO
import OsOnly(fixRootDir,fixTypeFile,fixObjectFile
             ,fixHatAuxFile,fixHatTransFile,fixHatFileBase)
import List(isPrefixOf)
import Char(isDigit)


data Flags = FF 
  {sRealFile   :: String
  ,sSourceFile :: String
  ,sTypeFile   :: String
  ,sObjectFile :: String
--  ,sHatAuxFile   :: String
--  ,sHatTransFile :: String
--  ,sHatFileBase  :: String
  ,sIncludes   :: [String]
  ,sPreludes   :: [String]

--v Flags to control compilation
  ,sRedefine   :: Bool	-- allow redefinitions of imported identifiers
  ,sPart       :: Bool	-- compiling part of a lib
  ,sUnix       :: Bool	-- either unix or RiscOS
  ,sUnlit      :: Bool	-- unliterate the source code
  ,sHiSuffix   :: String-- set the interface file suffix (usually .hi)

  ,nProfile    :: Int	-- turn on heap profiling
  ,sTprof      :: Bool	-- turn on time profiling
  ,sZap        :: Bool	-- zap unused args / stack positions
  ,sPrelude    :: Bool	-- keep prelude defns in interface file
  ,sLib        :: Bool	-- compiling a library
  ,sKeepCase   :: Bool	-- don't lift case, we fix those later

--v Flags to control compilation for tracing
  ,sDbgTrans   :: Bool	-- do tracing transformation
  ,sDbgPrelude :: Bool	-- use tracing prelude
  ,sDbgTrusted :: Bool	-- trust this module
--  ,sHatTrans   :: Bool	-- perform portable hat transformation for tracing

--v Flags for machine architecture / configuration
  ,sAnsiC      :: Bool	-- generate bytecode via ANSI-C
  ,s64bit      :: Bool	-- generate for a 64-bit machine (not currently used)
  ,sNplusK     :: Bool	-- allow (n+k) patterns
  ,sUnderscore :: Bool	-- force H'98 underscores
  ,sPuns       :: Bool	-- allow named-field puns

--v debugging flags - show program / import tables (after each compiler phase)
  ,sLex        :: Bool	-- input	after lexing
  ,sILex       :: Bool	-- input	after lexing imported interface files
  ,sParse      :: Bool	-- ast		after parsing
  ,sIParse     :: Bool	-- ast		after parsing imported interface files
  ,sNeed       :: Bool	-- need table	before imports
  ,sINeed      :: Bool	-- need table	after all imports
  ,sIINeed     :: Bool	-- need table	after each import
  ,sIRename    :: Bool	-- rename table	after all imports
  ,sIIRename   :: Bool	-- rename table	after each import
  ,sImport     :: Bool	-- imported filenames
  ,sRImport    :: Bool	-- imports 	actually used
  ,sDepend     :: Bool	-- imported ids	(not currently used)
  ,sRename     :: Bool	-- ast		after rename
  ,sDerive     :: Bool	-- ast		after deriving
  ,sTraceData  :: Bool	-- ast		after tracing transform (data)
  ,sTraceFns   :: Bool	-- ast		after tracing transform (fns)
  ,sRemove     :: Bool	-- ast		after named-field removal
  ,sScc        :: Bool	-- ast		after strongly-connected-components
  ,sType       :: Bool	-- ast		after type check
  ,sFixSyntax  :: Bool	-- ast		after removing newtypes
  ,sSTG        :: Bool	-- stg tree	after translation from ast
  ,sLift       :: Bool	-- stg tree	after lambda lifting
  ,sCase       :: Bool	-- stg tree	after case pattern simplification
  ,sPrim       :: Bool	-- stg tree	after inserting primitives
  ,sArity      :: Bool	-- stg tree	after arity analysis
  ,sBCBefore   :: Bool	-- stg tree	before conversion to byte code
  ,sAtom       :: Bool	-- stg tree	after only atoms in applications
  ,sSTGCode    :: Bool	-- stg code
  ,sFree       :: Bool	-- stg code	with explicit free variables

  ,sGcode      :: Bool	-- g-code
  ,sGcodeFix   :: Bool	-- g-code	after large constant fix
  ,sGcodeMem   :: Bool	-- g-code	after NEEDHEAP analysis
  ,sGcodeOpt1  :: Bool	-- g-code	after optimisation phase 1
  ,sGcodeRel   :: Bool	-- g-code	after relative offset analysis
  ,sGcodeOpt2  :: Bool	-- g-code	after optimisation phase 2

  ,sFunNames   :: Bool	-- insert position and name of functions in the code
  

--v debugging flags - show symbol table (after each compiler phase)
  ,sIBound     :: Bool	-- after all imports
  ,sIIBound    :: Bool	-- after each import
  ,sRBound     :: Bool	-- after rename
  ,sDBound     :: Bool	-- after deriving
  ,sEBound     :: Bool	-- after extract
  ,sTBound     :: Bool	-- after type checking
  ,sFSBound    :: Bool	-- after fixsyntax
  ,sLBound     :: Bool	-- after lambda-lifting
  ,sCBound     :: Bool	-- after case
  ,sPBound     :: Bool	-- after inserting prims
  ,sABound     :: Bool	-- after only atoms in applications

--v miscellaneous flags
  ,sShowType   :: Bool	-- report type of "main" (for hmake interactive)
  ,sShowWidth  :: Int   -- width for showing intermediate program
  ,sShowIndent :: Int   -- indentation for nesting shown intermediate program
  ,sShowQualified :: Bool -- show qualified ids as far as possible
  }
  deriving Show


-- not a selector, but a function:
sProfile :: Flags -> Bool
sProfile flags = nProfile flags > (0::Int)

  

{- If first argument is True, then print second and third with formatting -}
pF :: Bool -> [Char] -> [Char] -> IO ()
pF flag title text =
  if flag 
    then hPutStr stderr ( "======\t"++title++":\n"++text++"\n") 
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
     _ -> error ("\nusage: nhc98comp file.hs\n\ 
\       nhc98comp sourcefile interfacefile C-file\n\ 
\       nhc98comp sourcefile sourcename interface C-file\n")  

 flags = FF
  { sRealFile=realfile
  , sSourceFile=sourcefile
  , sTypeFile=typefile
  , sObjectFile=cfile
--  , sHatAuxFile=fixHatAuxFile isUnix rootdir filename
--  , sHatTransFile=fixHatTransFile isUnix rootdir filename
--  , sHatFileBase=fixHatFileBase isUnix rootdir filename
  , sIncludes=rootdir:getIncludes xs
  , sPreludes=getPreludes xs

  , sRedefine = fElem False "redefine" xs 
  -- ^ Don't complain if redefining an imported identifier
  , sPart = fElem False "part" xs      	        
  -- ^ Compiling part of a lib, so don't complain if module name differs 
  -- from file name and don't create 
  -- profiling information for this module
  , sUnix = fElem True  "unix" xs          	
  -- ^ Use unix file names
  , sUnlit = fElem False "unlit" xs         	
  -- ^ Unliterate the source code
  , sHiSuffix = stringFlag "hi" "hi-suffix=" xs
  -- ^ change the default ".hi" suffix
  , nProfile = length (filter (== "-profile") xs)	
  -- ^ amount of profiling information / node
  , sTprof = fElem False "tprof" xs    -- generate for time profiling PH
  , sZap = fElem True "zap" xs             	
  -- Generate code to zap unused arguments/stack positions
  , sPrelude = fElem False "prelude" xs		
  -- Keep prelude definitions in interface file
  , sLib = fElem False "lib" xs         
  -- ^ Compiling a lib, don't complain if importing modules with names 
  -- that differs from their filename.

  , sDbgTrans = fElem False "dbgtrans" xs     -- perform debugging translation
  , sDbgPrelude = fElem False "dbgprelude" xs -- use the debugging prelude
  , sDbgTrusted = fElem False "trusted" xs    -- "trusted" module (don't trace)
--  , sHatTrans = fElem False "hat" xs     -- preform hat tracing transform

  , sAnsiC = fElem True  "ansiC" xs    -- Generate bytecode as ANSI C file
  , s64bit = fElem False "64bit" xs    -- 32bit/64bit word size (ignored)
  , sNplusK = fElem False "nkpat" xs   -- Enable (n+k) patterns
  , sUnderscore = fElem False "underscore" xs 
  -- ^ Enable H'98 underscore-is-lower-case
  , sPuns  = fElem True  "puns" xs     -- Enable pre-98 named-field puns


  , sLex = fElem False "lex" xs         -- show lexical input
  , sParse  = fElem False "parse" xs    -- show syntax tree  after  parser
  , sNeed = fElem False "need" xs       -- show need   table before import
  , sINeed = fElem False "ineed" xs     -- show need   table after  import
  , sIRename = fElem False "irename" xs -- show rename table after  import
  , sIINeed = fElem False "iineed" xs   
  -- ^ show need   table between all import files
  , sIIRename = fElem False "iirename" xs
  -- ^ show rename table between all imports
  , sRename = fElem False "rename" xs   -- show syntax tree  after   rename
  , sDerive = fElem False "derive" xs   -- show syntax tree  after   derive
  , sRemove = fElem False "remove" xs   
  -- ^ show syntax tree  after fields are removed (translated into selectors)
  , sScc = fElem False "scc" xs               	
  -- ^ show syntax tree  after splitting into strongly connected groups
  , sType = fElem False "type" xs              	
  -- ^ show syntax tree  after type check
  , sFixSyntax = fElem False "fixsyntax" xs           
  -- ^ show syntax tree  after removing newtype constructors and 
  -- fixing Class.Type.metod
  , sSTG = fElem False "stg" xs	       
  -- ^ show stg    tree  after translation from syntax tree 
  , sLift = fElem False "lift" xs              	
  -- ^ show syntax tree  after lambda lifting
  , sCase = fElem False "case" xs              	
  -- ^ show stg    tree  after simplification of patterns
  , sPrim = fElem False "prim" xs              	
  -- ^ show stg    tree  after inserting primitive functions
  , sBCBefore = fElem False "bcbefore" xs		
  -- ^ show stg    tree  before converting to byte code


  , sGcode = fElem False "gcode" xs            	
  -- ^ show G code    	  -- NR
  , sGcodeFix = fElem False "gcodefix" xs            
  -- ^ show G code after large constant fixed -- NR
  , sGcodeMem = fElem False "gcodemem" xs     -- show G code NEEDHEAP
  , sGcodeOpt1 = fElem False "gcodeopt1" xs   -- show G code optimisation
  , sGcodeRel = fElem False "gcoderel" xs     -- show G code after offsets -- NR
  , sKeepCase = fElem False "keepcase" xs     
  -- ^ Don't lift case, we fix those later
  , sArity = fElem False "arity" xs           -- show stg    tree  after arity
  , sSTGCode = fElem False "stgcode" xs       -- show STG code	-- DW
  , sGcodeOpt2 = fElem False "gcodeopt2" xs   -- show G code optimisation


  , sIBound = fElem False "ibound" xs   -- show symbol table after  import
  , sIIBound = fElem False "iibound" xs 
  -- ^ show symbol table between all import files
  , sRBound = fElem False "rbound" xs   -- show symbol table after   rename
  , sDBound = fElem False "dbound" xs   -- show symbol table after   derive
  , sPBound = fElem False "pbound" xs   
  -- ^ show symbol table after inserting primitive functions
  , sEBound = fElem False "ebound" xs   -- show symbol table after extract
  , sTBound = fElem False "tbound" xs   -- show symbol table after type check
  , sFSBound = fElem False "fsbound" xs            	
  -- ^ show symbol table after adding Class.Type.metod info
  , sLBound = fElem False "lbound" xs            	
  -- ^ show symbol table after lambda lifting
  , sCBound = fElem False "cbound" xs            	
  -- ^ show symbol table after simplification of pattern
  , sABound = fElem False "abound" xs		
  -- ^ show symbol table after only atoms in applications


  , sImport = fElem False "import" xs         -- print name of imported files
  , sDepend = fElem False "depend" xs	     
  -- ^ print imported identifiers that are used (not even alpha yet)
  , sFree = fElem False "free" xs	     
  -- ^ show stg    tree  with explicyr free variables
  , sAtom = fElem False "atom" xs		
  -- ^ show stg    tree  after only atoms in applications
  , sFunNames = fElem False "funnames" xs            
  -- ^ insert position and name of functions in the code
  , sILex = fElem False "ilex" xs             -- show lexical input
  , sIParse = fElem False "iparse" xs         -- show syntax tree  after  parser
  , sRImport = fElem False "report-imports" xs	
  -- ^ show only imports actually used

  , sTraceData = fElem False "tracedata" xs	      	     
  -- ^ show ast after debugging translation for data
  , sTraceFns = fElem False "tracefns" xs  -- ast after transforming functions

  , sShowType = fElem False "showtype" xs  -- report type of "main"

  , sShowWidth = cardFlag 80 "showwidth=" xs  -- set width for showing 
                                              -- intermediate program
  , sShowIndent = cardFlag 2 "showindent=" xs -- set indentation for nesting
  , sShowQualified = fElem True "showqualified" xs  
  -- ^ show qualified ids as far as possible
  }
  
  
{- obtain list of filenames from argument list -}
getFiles :: [String] -> [String]
getFiles = filter (\xs -> case xs of ('-':_) -> False ; _ -> True)


{- obtain list of include paths from argument list -}
getIncludes :: [String] -> [String]
getIncludes = map (drop (2::Int)) . 
              filter (\xs -> case xs of ('-':'I':_) -> True  
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


{-
Returns the value of an option with a numerical (cardinal) value.
If the option is not given, then the default value (first arg.) is returned.
Ignores syntactically incorrect options.
-}
cardFlag :: Int -> [Char] -> [String] -> Int
cardFlag def f flags = if null settings then def else read (last settings)
  where
  settings = filter (all isDigit) . map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags



{-
Returns the value of a "-something=" option with a string value.
If the option is not given, then the default value (first arg.) is returned.
-}
stringFlag :: String -> String -> [String] -> String
stringFlag def f flags = if null settings then def else last settings
  where
  settings = map (drop (length f + 1)) . 
             filter (isPrefixOf ('-':f)) $ flags
