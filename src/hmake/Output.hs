module Output(qCompile,qLink,qCleano,qCleanhi) where
import ListUtil -- (lconcatMap)
import FileName
import ListUtil
import Argv
--import Maybe

doEcho True cmd = "echo \"" ++ cmd ++ "\"\n" ++ cmd ++ "\n"
doEcho False cmd = cmd ++ "\n"

oFile,hiFile :: DecodedArgs -> String -> String -> String
oFile opts path mod =
    let g    = goalDir opts
        gDir = if null g then path else g
    in fixFile opts gDir mod (oSuffix opts)
hiFile opts path mod  =
       fixFile opts path mod (hiSuffix opts)

cleanModuleName (Program file)    = file
cleanModuleName (Object file suf) = file

qCleano  opts echo graph mod =
  let allfiles = close graph [] [cleanModuleName mod]
  in doEcho echo ("rm -f" ++
         concatMap (\(d,f)-> ' ': oFile opts d f) allfiles)

qCleanhi opts echo graph mod =
  let allfiles = close graph [] [cleanModuleName mod]
  in doEcho echo ("rm -f" ++
         concatMap (\(d,f)-> ' ': hiFile opts d f) allfiles)

qCompile opts echo (dep,(p,m,source,cpp)) =
  if null dep
  then doEcho echo compilecmd
  else test m dep compilecmd
 where
  ofile = oFile opts p m
  compilecmd =
    compiler ++ "-c " ++ cppcmd ++
    (if (dflag opts) then "-d "++(goalDir opts) else "-o "++ofile) ++
    " " ++ source

  compiler = if (isUnix opts) then "${HC} ${HFLAGS} " else "nhc98 "
  cppcmd = if cpp then "-cpp"++concat (map doD (defs opts))++" " else ""
  doD s = " -D"++s

  test = if (isUnix opts)
         then (\m dep comp ->
               "if [ `$OLDER " ++ ofile
		++ lconcatMap (\(d,p) -> ' ':hiFile opts p d) dep
	        ++"` = 1 ]\nthen\n"
	        ++ doEcho echo compilecmd
 	        ++ "fi\n")
         else (\m dep comp ->
               "older " ++ ofile
                        ++ lconcatMap (\(d,p) -> ' ':hiFile opts p d) dep
                ++ "\nset Nhc$ReturnCode <Sys$ReturnCode>\n"
           --   ++ (if echo then "IF <Nhc$ReturnCode> THEN echo " ++ compilecmd
           --               else [])
                ++ "IF <Nhc$ReturnCode> THEN " ++ compilecmd)


qLink opts echo graph (Object  file suf) = ""
qLink opts echo graph (Program file)     =
  cmd
 where
  goaldir = goalDir opts
  goal = if null goaldir then "." else goaldir
  mkOfile path f = if (dflag opts) then
                        fixFile opts ""   f (oSuffix opts)
                   else fixFile opts path f (oSuffix opts)
  objfiles = close graph [] [file]
  cmd | isUnix opts =
	  let objs =  lconcatMap (\(d,f) -> ' ':mkOfile d f) objfiles in
          if null goaldir then
	    "if [ `$OLDER "++file++" "++objs++"` = 1 ]\nthen\n"
	     ++ doEcho echo ("${HC} ${HFLAGS}"++" -o "
                             ++file++objs++" ${LDFLAGS}")
	     ++ "fi\n"
          else
	    "if ( cd "++goaldir++" && [ `$OLDER "
             ++ file ++ " "++objs++"` = 1 ] )\nthen\n"
	     ++ doEcho echo ("cd "++goal++" && ${HC} ${HFLAGS}"++" -o "
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
        ((tps,d,s,_),new) -> 
          close graph ((d,f):acc) (fs ++ new)


