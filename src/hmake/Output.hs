module Output(qCompile,qLink,qCleano,qCleanhi) where

import ListUtil -- (lconcatMap)
import FileName
import List (intersperse)
import Argv
import PreProcessor
import Config
import RunAndReadStdout (basename)

doEcho True cmd = "echo \"" ++ cmd ++ "\"\n" ++ cmd ++ "\n"
doEcho False cmd = cmd ++ "\n"

oFile,hiFile,hatFile :: DecodedArgs -> String -> String -> String
oFile opts path fmodule =
    let g    = goalDir opts
        gDir = if null g then path else g
        tmod = if hat opts then ("Hat/"++) else id
    in fixFile opts gDir (tmod fmodule) (oSuffix opts)
hiFile opts path fmodule =
       fixFile opts path fmodule (hiSuffix opts)
--iFile opts path fmodule =
--       fixFile opts path fmodule ("pp.hs")
hatFile opts path fmodule  =
       fixFile opts path ("Hat/"++fmodule) ("hs")
hatHiFile opts path fmodule  =
       fixFile opts path ("Hat/"++fmodule) (hiSuffix opts)
hxFile opts path fmodule  =
       fixFile opts path fmodule ("hx")

cleanModuleName (Program file)    = file
cleanModuleName (Object file suf) = file

qCleano  opts echo graph mod =
  let allfiles = close graph [] [cleanModuleName mod]
  in doEcho echo ("rm -f" ++
         concatMap (\(d,f)-> ' ': oFile opts d f) allfiles)

qCleanhi opts echo graph mod =
  let allfiles = close graph [] [cleanModuleName mod]
  in if hat opts then
         doEcho echo ("rm -f" ++
             concatMap (\(d,f)-> ' ': hatHiFile opts d f) allfiles) ++
         doEcho echo ("rm -f" ++
             concatMap (\(d,f)-> ' ': hatFile opts d f) allfiles) ++
         doEcho echo ("rm -f" ++
             concatMap (\(d,f)-> ' ': hxFile opts d f) allfiles)
     else
         doEcho echo ("rm -f" ++
             concatMap (\(d,f)-> ' ': hiFile opts d f) allfiles)

qCompile opts echo (dep,(p,m,srcfile,cpp,pp)) =
  test dep (preprocess++hattrans++compilecmd)
 where
  -- srcfile -(preprocess)-> pfile -(hattrans)-> hfile -(compile)-> ofile
  ofile = oFile opts p m
  pfile
    | null (ppExecutableName pp) = srcfile
    | otherwise = fixFile opts p m "hs"
  hfile
    | hat opts  = hatFile opts p m
    | otherwise = pfile
  preprocess
    | null (ppExecutableName pp) = ""
    | otherwise = doEcho echo $
                  ppExecutableName pp++" "
                    ++concat (intersperse " " (ppDefaultOptions pp opts
                                               ++[ppOutputFileOption pp pfile]
                                               ++[srcfile]))
  hattrans
    | hat opts && cpp =
            doEcho echo ("gcc -E -traditional -x c "++pfile
                        ++concatMap doD (defs opts ++ zdefs opts)
                        ++" -o /tmp/"++basename pfile)
            ++ doEcho echo ("hat-trans $HATFLAGS -P. /tmp/"++basename pfile
				++" "++pfile)
         -- ++ doEcho echo ("mv "++hatFile opts "/tmp" m++" "++hfile)
         -- ++ doEcho echo ("mv "++hxFile opts "/tmp" m++" "++hxFile opts p m)
    | hat opts && not cpp = doEcho echo $
                            "hat-trans $HATFLAGS "++pfile
    | otherwise = ""
  compilecmd = doEcho echo $
    hc ++ "-c " ++ cppcmd
    ++ (if hat opts then "-package hat " else " ")
    ++ (if (dflag opts) then "-d "++goalDir opts++" " else "-o "++ofile++" ")
    ++ hfile

  hc | isUnix opts = compilerPath (compiler opts)++" ${HFLAGS} "
     | otherwise   = compilerPath (compiler opts)
  cppcmd = if cpp then "-cpp"++concatMap doD (defs opts)++" " else ""
  doD s = " -D"++s

  test []  comp = comp
  test dep comp
    | isUnix opts = "if [ `$OLDER " ++ ofile
                    ++ lconcatMap (\(d,p) -> ' ':hiFile opts p d) dep
                    ++"` = 1 ]\nthen\n"
                    ++ comp
                    ++ "\nfi\n"
    | otherwise = "older " ++ ofile
                           ++ lconcatMap (\(d,p) -> ' ':hiFile opts p d) dep
                   ++ "\nset Nhc$ReturnCode <Sys$ReturnCode>\n"
                   ++ "IF <Nhc$ReturnCode> THEN " ++ comp


qLink opts echo graph (Object  file suf) = ""
qLink opts echo graph (Program file)     =
  cmd
 where
  goaldir = goalDir opts
  goal = if null goaldir then "." else goaldir
  tmod = if hat opts then ("Hat/"++) else id
  mkOfile path f = if (dflag opts) then
                        fixFile opts ""   (tmod f) (oSuffix opts)
                   else fixFile opts path (tmod f) (oSuffix opts)
  objfiles = close graph [] [file]
  hatflag = if hat opts then "-package hat " else ""
  hc | isUnix opts = compilerPath (compiler opts)++" ${HFLAGS} "
     | otherwise   = compilerPath (compiler opts)
  cmd | isUnix opts =
	  let objs =  lconcatMap (\(d,f) -> ' ':mkOfile d f) objfiles in
          if null goaldir then
	    let objs =  lconcatMap (\(d,f) -> ' ':mkOfile d f) objfiles in
	    "if [ `$OLDER "++file++" "++objs++"` = 1 ]\nthen\n"
	     ++ doEcho echo (hc++hatflag++" -o "++file++objs++" ${LDFLAGS}")
	     ++ "fi\n"
          else
	    let objs = lconcatMap (\(d,f) -> ' ':
                                      fixFile opts "" (tmod f) (oSuffix opts))
                                  objfiles in
	    "if ( cd "++goaldir++" && [ `$OLDER "
             ++     file ++ " "++objs++"` = 1 ] )\nthen\n"
	     ++ doEcho echo ("cd "++goal++" && "++hc++hatflag++" -o "
                             ++file++objs++" ${LDFLAGS}")
	     ++ "fi\n"
      | otherwise =
          if length objfiles > 3 then
             "exfile <Wimp$ScrapDir>.nhcmk_via STOP\n"
              ++ lconcatMap (\(d,f) ->
                             ' ': fixFile opts (if null d then goal else d)
                                          f (oSuffix opts)
                                ++ "\n")
                             objfiles
              ++ "STOP\n"
              ++ "nhc98 " ++ " -o " ++ file
              ++ " -via <Wimp$ScrapDir>.nhcmk_via\n"
          else
             "nhc98 " ++ " -o " ++ file
              ++ lconcatMap (\(d,f) ->
                             ' ': fixFile opts (if null d then goal else d)
                                          f (oSuffix opts))
                            objfiles
              ++ "\n"



close graph acc []      = acc
close graph acc (f:fs)  =
    if any ((f==).snd) acc then
      close graph acc fs
    else
      case assocDef graph (error "Use?") f of
        ((tps,d,s,_,_),new) -> 
          close graph ((d,f):acc) (fs ++ new)


