module Output(qCompile,qLink,qCleano,qCleanhi) where
import ListUtil -- (lconcatMap)
import FileName
import ListUtil
import Argv  -- Goal
--import Maybe

doEcho True cmd = "echo \"" ++ cmd ++ "\"\n" ++ cmd ++ "\n"
doEcho False cmd = cmd ++ "\n"

oFile goalDir unix path mod =
    if null goalDir then fixFile unix path    mod "o"
                    else fixFile unix goalDir mod "o"
hiFile unix path mod  =  fixFile unix path    mod "hi"

qCleano  echo goalDir unix (_,(p,m,source,cpp)) =
    doEcho echo ("rm -f " ++ oFile goalDir unix p m)
qCleanhi echo unix (_,(p,m,source,cpp)) =
    doEcho echo ("rm -f " ++ hiFile unix p m)

qCompile echo goalDir dflag defines unix (dep,(p,m,source,cpp)) =
  if null dep
  then doEcho echo compilecmd
  else test m dep compilecmd
 where
  ofile = oFile goalDir unix p m
  compilecmd =
    compiler ++ "-c " ++ cppcmd ++
    (if dflag then "-d "++goalDir else "-o "++ofile) ++
    " " ++ source

  compiler = if unix then "${HC} ${HFLAGS} " else "nhc98 "
  cppcmd = if cpp then "-cpp"++concat (map doD defines)++" " else ""
  doD s = " -D"++s

--goal = if goalDir=="" then "." else goalDir
--compile source = compiler++" -c "++source
--compile source path mod =
--  compiler ++ " -c " ++ cppcmd ++ source {-fixFile unix path mod "hs"-}
--  ++ (if dflag || (path==goal) || (path/=".") then ""
--      else " -o " ++ fixFile unix goalDir mod "o")


  test = if unix
         then (\m dep comp ->
               "if [ `$OLDER " ++ ofile
		++ lconcatMap (\(d,p) -> ' ':hiFile unix p d) dep
	        ++"` = 1 ]\nthen\n"
	        ++ doEcho echo compilecmd
 	        ++ "fi\n")
         else (\m dep comp ->
               "older " ++ ofile
                        ++ lconcatMap (\(d,p) -> ' ':hiFile unix p d) dep
                ++ "\nset Nhc$ReturnCode <Sys$ReturnCode>\n"
--                ++ (if echo then "IF <Nhc$ReturnCode> THEN echo " ++ compilecmd else [])
                ++ "IF <Nhc$ReturnCode> THEN " ++ compilecmd)

--test = if unix
--       then (\m dep comp ->
--             "if [ `$OLDER " ++ fixFile unix goalDir m "o" 
--		++ lconcatMap (\d -> ' ':fixFile unix p d "hi") dep
--              ++"` = 1 ]\nthen\n"
--     		++ doEcho echo compilecmd
--     		++ "fi\n")
--       else (\m dep comp ->
--             "older " ++ fixFile unix goalDir m "o" 
--              ++ lconcatMap (\d -> ' ':fixFile unix p d "hi") dep
--              ++ "\nset Nhc$ReturnCode <Sys$ReturnCode>\n"
----            ++ (if echo then "IF <Nhc$ReturnCode> THEN echo " ++ compilecmd else [])
--              ++ "IF <Nhc$ReturnCode> THEN " ++ compilecmd)



qLink echo goalDir dflag unix graph (Object  file) = ""
qLink echo goalDir dflag unix graph (Program file) =
  cmd
 where
  goal = if goalDir=="" then "." else goalDir
  mkOfile path f = if dflag then fixFile unix ""   f "o"
                            else fixFile unix path f "o"
  cmd = if unix 
        then
	  let objs =  lconcatMap (\(d,f) -> ' ':mkOfile d f) objfiles in
          if null goalDir then
	    "if [ `$OLDER "++file++" "++objs++"` = 1 ]\nthen\n"
	     ++ doEcho echo ("${HC} ${HFLAGS}"++" -o "++file++objs++" ${LDFLAGS}")
	     ++ "fi\n"
          else
	    "if ( cd "++goalDir++" && [ `$OLDER "++file++" "++objs++"` = 1 ] )\nthen\n"
	     ++ doEcho echo ("cd "++goal++" && ${HC} ${HFLAGS}"++" -o "++file++objs++" ${LDFLAGS}")
	     ++ "fi\n"
        else 
          if length objfiles > 3 then
             "exfile <Wimp$ScrapDir>.nhcmk_via STOP\n"
              ++ lconcatMap (\(d,f) -> ' ':fixFile unix (if null d then goal else d) f "o" ++ "\n") objfiles
              ++ "STOP\n"
              ++ "nhc98 " ++ " -o " ++ file ++ " -via <Wimp$ScrapDir>.nhcmk_via\n"
          else
             "nhc98 " ++ " -o " ++ file
                      ++ lconcatMap (\(d,f) -> ' ':fixFile unix (if null d then goal else d) f "o") objfiles
                      ++ "\n"

--qLink echo goalDir dflag unix graph (Object  file) = ""
--qLink echo goalDir dflag unix graph (Program file) =
--  cmd
-- where
--  goal = if goalDir=="" then "." else goalDir
--  cmd = if unix 
--        then
--	  let objs =  lconcatMap (\(d,f) -> ' ':fixFile unix (if null d then "" else d) f "o") objfiles
--	--let objs =  lconcatMap (\(d,f) -> ' ':fixFile unix "" f "o") objfiles
--	  in "if cd "++goal++" && [ `$OLDER "++file++" "++objs++"` = 1 ]\nthen\n"
--	     ++ doEcho echo ("cd "++goal++" && ${HC} ${HFLAGS}"++" -o "++file++objs++" ${LDFLAGS}")
--	     ++ "fi\n"
--        else 
--          if length objfiles > 3 then
--             "exfile <Wimp$ScrapDir>.nhcmk_via STOP\n"
--              ++ lconcatMap (\(d,f) -> ' ':fixFile unix (if null d then goal else d) f "o" ++ "\n") objfiles
--              ++ "STOP\n"
--              ++ "nhc98 " ++ " -o " ++ file ++ " -via <Wimp$ScrapDir>.nhcmk_via\n"
--          else
--             "nhc98 " ++ " -o " ++ file
--                      ++ lconcatMap (\(d,f) -> ' ':fixFile unix (if null d then goal else d) f "o") objfiles
--                      ++ "\n"


  objfiles = close graph [] [file]

  close graph acc []      = acc
  close graph acc (f:fs)  =
    if any ((f==).snd) acc then
      close graph acc fs
    else
      case assocDef graph (error "Use?") f of
        ((tps,d,s,_),new) -> 
          close graph ((d,f):acc) (fs ++ new)


