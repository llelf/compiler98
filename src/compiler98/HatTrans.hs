{- ---------------------------------------------------------------------------
This `main' function is basically just the front-end of the nhc98
compiler.  It parses the .hs source file, creates the .hx file, and
then stops immediately after writing a new transformed .hs file.
-} 
module Main where

import IO
import System
import Monad(when)

import Error
import Syntax
import Extra(Pos(..),mix,mixSpace,jRight,jLeft,noPos,strPos,showErr,mixLine,
             pair,fst3,thd3,trace)
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
import PrettySyntax(prettyPrintTokenId,prettyPrintId,prettyPrintTraceId
                   ,ppModule,ppTopDecls,ppClassCodes)

import TokenId(TokenId(..),t_Arrow,t_List,tPrelude,tminus,tnegate,tTrue)
import IdKind(IdKind(..))
import Id(Id)
import Lex(Lex,LexAnnot)  -- need show

import Unlit(unlit)
import Lexical(PosToken(..),PosTokenPre(..),LexState(..),lexical)
import Parse(parseProg)

import AuxFile(toAuxFile)
import AuxLabelAST(auxLabelSyntaxTree)
import TraceTrans(traceTrans,maybeStripOffQual)


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
                          ("In file: "++sSourceFile flags) showErr
  pF (sParse flags) "Parse" (prettyPrintTokenId flags ppModule parsedPrg) 


  {-
  -- Read and write auxiliary information files (for tracing).
  -- Then relabel the syntax tree with the auxiliary information.
  -- Then the tracing transformation itself is applied.
  -- The result is written to file (no redirection possible yet)
  -}
  let prg = maybeStripOffQual "Prelude" parsedPrg
  toAuxFile flags (sHatAuxFile flags) prg
  when (sParse flags)
       (putStr (prettyPrintTokenId flags ppModule prg)) -- debug
  newprog <- auxLabelSyntaxTree flags prg
  when (sTraceFns flags)
       (putStr (prettyPrintTraceId flags ppModule newprog)) -- debug
  writeFile (sHatTransFile flags)
      (prettyPrintTokenId flags ppModule 
        (maybeStripOffQual "TPrelude"
          (traceTrans (not (sDbgTrusted flags)) (sSourceFile flags) 
            (sHatFileBase flags) newprog)))
  putStrLn ("Wrote " ++ sHatTransFile flags)
  exitWith (ExitSuccess)

{- End Module Main ----------------------------------------------------------}
