module Flags where

import IO

type Flags = (FlagFiles,FlagBools)

type FlagFiles =
	(String,String,String,String,[String],[String]) 

type FlagBools = 
	((Bool, Bool, Bool, Bool, Int,  Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, (),   ()  )
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, Bool, Bool, Bool, Bool)
	,(Bool, Bool, Bool, ()  , ()  , ()  , ()  )
	,(Bool, Bool, Bool, Bool, Bool, ()  , ()  )
	)

pF flag title text =
    if flag then hPutStr stderr ("\n====================================\n\t" ++ title++":\n"++text++"\n") else return ()

getFiles :: [String] -> [String]
getFiles = filter (\xs -> case xs of ('-':_) -> False ; _ -> True)

getIncludes :: [String] -> [String]
getIncludes = map (drop (2::Int)) . filter (\xs -> case xs of ('-':'i':_) -> True ; ('-':'I':_) -> True ; _ -> False)

getPreludes :: [String] -> [String]
getPreludes = map (drop (2::Int)) . filter (\xs -> case xs of ('-':'P':_) -> True ; _ -> False)

getFlags :: [String] -> FlagBools
getFlags xs = (
  (fElem False "redefine" xs		-- Don't complain if redefining an imported identifier
  ,fElem False "part" xs      	        -- Compiling part of a lib, so don't complain if module name differs from file name and don't create 
					-- profiling information for this module
  ,fElem True  "unix" xs          	-- Use unix file names
  ,fElem False "unlit" xs         	-- Unliterate the source code
  ,length (filter (== "-profile") xs)	-- amount of profiling information / node
  ,fElem True "zap" xs             	-- Generate code to zap unused arguments/stack positions
  ,fElem False "prelude" xs		-- Keep prelude definitions in interface file
  ),
  (fElem True  "ansiC" xs		-- Generate bytecode as ANSI C file
  ,fElem False "64bit" xs		-- 32bit/64bit word size (ignored)
  ,fElem False "tprof" xs		-- generate for time profiling PH
  ,fElem False "nkpat" xs		-- Enable (n+k) patterns
  ,fElem False "underscore" xs		-- Enable H'98 underscore-is-lower-case
  ,()
  ,()
  ),
  (fElem False "lex" xs               	-- show lexical input
  ,fElem False "parse" xs             	-- show syntax tree  after  parser
  ,fElem False "need" xs              	-- show need   table before import
  ,fElem False "ineed" xs	     	-- show need   table after  import
  ,fElem False "irename" xs	      	-- show rename table after  import
  ,fElem False "ibound" xs            	-- show symbol table after  import
  ,fElem False "lib" xs                 -- Compiling a lib, don't complain if importing  modules with names that differs from their filename.
  ),
  (fElem False "iineed" xs	      	-- show need   table between all import files
  ,fElem False "iibound" xs	      	-- show symbol table between all import files
  ,fElem False "rename" xs            	-- show syntax tree  after   rename
  ,fElem False "rbound" xs             	-- show symbol table after   rename
  ,fElem False "derive" xs            	-- show syntax tree  after   derive
  ,fElem False "dbound" xs            	-- show symbol table after   derive
  ,fElem False "pbound" xs		-- show symbol table after inserting primitive functions
  ),
  (fElem False "ebound" xs            	-- show symbol table after extract
  ,fElem False "remove" xs  	      	-- show syntax tree  after fields are removed (translated into selectors)
  ,fElem False "scc" xs               	-- show syntax tree  after splitting into strongly connected groups
  ,fElem False "type" xs              	-- show syntax tree  after type check
  ,fElem False "tbound" xs            	-- show symbol table after type check
  ,fElem False "fixsyntax" xs           -- show syntax tree  after removing newtype constructors and fixing Class.Type.metod
  ,fElem False "stg" xs			-- show stg    tree  after translation from syntax tree 
  ),
  (fElem False "fsbound" xs            	-- show symbol table after adding Class.Type.metod info
  ,fElem False "lift" xs              	-- show syntax tree  after lambda lifting
  ,fElem False "lbound" xs            	-- show symbol table after lambda lifting
  ,fElem False "case" xs              	-- show stg    tree  after simplification of patterns
  ,fElem False "cbound" xs            	-- show symbol table after simplification of pattern
  ,fElem False "prim" xs              	-- show stg    tree  after inserting primitive functions
  ,fElem False "bcbefore" xs		-- show stg    tree  before converting to byte code
  ),
  (fElem False "gcode" xs            	-- show G code    	  -- NR
  ,fElem False "gcodefix" xs            -- show G code after large constant fixed -- NR
  ,fElem False "gcodemem" xs            -- show G code NEEDHEAP
  ,fElem False "gcodeopt1" xs           -- show G code optimisation
  ,fElem False "gcoderel" xs            -- show G code after offsets -- NR
  ,fElem False "keepcase" xs            -- Don't lift case, we fix those later
  ,fElem False "arity" xs               -- show stg    tree  after arity
  ),
  (fElem False "stgcode" xs            	-- show STG code	-- DW
  ,fElem False "gcodeopt2" xs          	-- show G code optimisation
  ,fElem False "import" xs              -- print name of imported files
  ,fElem False "depend" xs		-- print imported identifiers that are used (not even alpha yet)
  ,fElem False "free" xs		-- show stg    tree  with explicyr free variables
  ,fElem False "atom" xs		-- show stg    tree  after only atoms in applications
  ,fElem False "abound"	xs		-- show symbol table after only atoms in applications
  ),
  (fElem False "funnames" xs            -- insert position and name of functions in the code
  ,fElem False "ilex" xs               	-- show lexical input
  ,fElem False "iparse" xs             	-- show syntax tree  after  parser
  ,()
  ,()
  ,()
  ,()
  ),
  (
   fElem False "dbgtrans" xs	      -- perform debugging translation
  ,fElem False "dbg" xs	      	      -- show ast after debugging translation
  ,fElem False "dbg2" xs	      -- more debugging output
  ,fElem False "dbgprelude" xs        -- use the debugging prelude
  ,fElem False "trusted" xs           -- A "trusted" module (don't trace)
  ,()
  ,()
  )
  )

fElem def f flags = if ('-':f) `elem` flags then True
                    else if ('-':'n':'o':f) `elem` flags then False
                    else def


sRealFile    ((n,_,_,_,_,_),_) = n
sSourceFile  ((_,s,_,_,_,_),_) = s
sTypeFile    ((_,_,t,_,_,_),_) = t
sObjectFile  ((_,_,_,o,_,_),_) = o
sIncludes    ((_,_,_,_,i,_),_) = i
sPreludes    ((_,_,_,_,_,p),_) = p

--- Flags to control compilation

sRedefine  (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = a
sPart      (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = b
sUnix      (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = c
sUnlit     (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = d

nProfile   (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = e
sProfile   (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = e > (0::Int)
sZap       (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = f
sPrelude   (_,((a,b,c,d,e,f,g),_,_,_,_,_,_,_,_,_)) = g

-- Flags for machine architecture

sAnsiC     (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = a
s64bit     (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = b
sTprof     (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = c
sNplusK    (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = d
sUnderscore(_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = e
--FREE     (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = f
--FREE     (_,(_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_,_)) = g

--- debugging flags

sLex       (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = a
sParse     (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = b
sNeed      (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = c
sINeed     (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = d
sIRename   (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = e
sIBound    (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = f
sLib       (_,(_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_,_)) = g  -- not a debugging flag, but it was free

sIINeed    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = a
sIIBound   (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = b
sRename    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = c
sRBound    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = d
sDerive    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = e
sDBound    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = f
sPBound    (_,(_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_,_)) = g
sEBound    (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = a
sRemove	   (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = b
sScc       (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = c
sType      (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = d
sTBound    (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = e
sFixSyntax (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = f
sSTG       (_,(_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_,_)) = g
sFSBound   (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = a
sLift      (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = b
sLBound    (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = c
sCase      (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = d
sCBound    (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = e
sPrim      (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = f
sBCBefore  (_,(_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_,_)) = g

sGcode     (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = a
sGcodeFix  (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = b
sGcodeMem  (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = c
sGcodeOpt1 (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = d
sGcodeRel  (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = e
sKeepCase  (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = f
sArity     (_,(_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_,_)) = g

sSTGCode   (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = a
sGcodeOpt2 (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = b
sImport    (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = c
sDepend    (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = d
sFree      (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = e
sAtom      (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = f
sABound    (_,(_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_,_)) = g

sFunNames  (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = a
sILex      (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = b
sIParse    (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = c
--FREE     (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = d
--FREE     (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = e
--FREE     (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = f
--FREE     (_,(_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g),_)) = g

sDbgTrans     (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = a
sDbg	      (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = b
sDbg2	      (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = c
sDbgPrelude   (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = d
sDbgTrusted   (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = e
--FREE        (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = f
--FREE        (_,(_,_,_,_,_,_,_,_,_,(a,b,c,d,e,f,g))) = g





