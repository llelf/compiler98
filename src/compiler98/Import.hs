module Import (importOne) where

import IO
import PackedString(PackedString,packString,unpackPS)
import Flags
import OsOnly(fixImportNames,isPrelude)
import Extra
import Syntax(Module,ImpDecl,Type,Decls,Decl,FixId,InfixClass(..),Simple,Context,Constr)
import Lex(Lex)
import TokenId(TokenId(..),tPrelude,rpsPrelude,extractV)
import ParseCore(Parser(..),ParseBad(..),ParseError(..),ParseGood(..),ParseResult(..)
                 ,parseit)
import ParseI
import Lexical(PosToken(..),PosTokenPre(..),LexState(..),lexical)
import State
import Error
import IExtract
import ImportState(ImportState,putModid2IS)
import IntState(dummyIntState)
import PPSyntax(ppModule,ppDecl,ppDecls,ppImpDecls,ppInterface,ppFun,ppClassCodes)

#if !defined(__HASKELL98__)
#define ioError fail
#endif

openImport :: Flags -> PackedString -> IO (String,String,String)
openImport flags mrps =
 catch (do
         (fstr,finput) <- readFirst filenames 
         if sImport flags then hPutStr stderr ("Importing module " ++ mstr ++ " from " ++ fstr ++ ".\n") else return ()
         return (mstr,fstr,finput))
       (\ err -> ioError (userError (can'tOpenStr mstr filenames err)))
 where
   isUnix = sUnix flags
   preludes = sPreludes flags
   includes = sIncludes flags ++ preludes
   mstr = (reverse . unpackPS)  mrps
   filenames = fixImportNames isUnix mstr (if isPrelude mstr then preludes else includes) 

readFirst :: [String] -> IO (String,String)
readFirst []     = hPutStr stderr "Fail no filenames, probably no -I or -P" >> exit
readFirst [x]    =
    do finput <- readFile x
       return (x,finput)
readFirst (x:xs) =
    catch (do finput <- readFile x
	      return (x,finput))
	  (\ _ -> readFirst xs)


importOne flags importState (mrps,needFun,hideFun) =
  openImport flags mrps >>= \ (mstr,fstr,finput) ->
  let lexdata = lexical (sUnderscore flags) fstr finput
  in pF (sILex flags) "Lexical Interface" (mixSpace (map (\ (p,l,_,_) -> strPos p ++ ':':show l) lexdata)) >>
    case parseit parseInterface1 lexdata of
    Left err -> parseIError fstr err
    Right (modid,imports,fixity,rest) ->
      (if not (sLib flags || sPart flags) && show modid /= mstr
       then hPutStr stderr ("Warning: The module " ++ mstr ++ " is called " ++ show modid
				  ++ " in its interface file (" ++ fstr ++")\n")
       else return () ) >>
      case parseit (parseInterface2 (needFixity fixity (putModidIS importState (extractV modid))) hideFun) rest of
        Left err -> parseIError fstr err
	Right (importState,need,rest) ->
	  importCont'  importState needFun hideFun mstr fstr need rest


--                   needFun
-- down ((Memo TokenId -> [[TokenId]] -> Bool)


importCont' importState needFun hideFun modid filename need rest =
   importCont (Right (ParseNeed importState need rest))
 where

  importCont (Left err) = parseIError filename err
  importCont (Right (ParseEof  importState)) =  return importState :: (IO ImportState)
  importCont (Right (ParseNext importState visible (pos,Visible mrps) rest)) =
    importCont (parseit (parseUntilNeed (putModid2IS importState visible mrps)) rest)
  importCont (Right (ParseNeed importState (Just needs@(_:_)) rest)) =
     if needFun (getNeedIS importState) needs
     then importCont (parseit (parseInterface3 importState hideFun) rest)
     else importCont (parseit (parseUntilNeed importState) rest)

  importCont (Right (ParseNeed importState (Just []) rest)) =
     importCont (parseit (parseInterface3 importState hideFun) rest)

  importCont (Right (ParseNeed importState _ rest)) =
     importCont2 (parseit (parseInterface4 importState hideFun) rest)

  importCont2 (Left err) = parseIError filename err
  importCont2 (Right (ParseEof importState)) = return importState
  importCont2 (Right (ParseNext importState visible (pos,Visible mrps) rest)) =
    importCont (parseit (parseUntilNeed (putModid2IS importState visible mrps)) rest)


parseIError filename err = (ioError . userError . errorStr filename . showErr) err

