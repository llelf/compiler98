import HatTrace
import HatTrie
import HatExpressionTree
import Maybe
import System
import Char(isDigit,digitToInt,toUpper)
import IO(hFlush,stdout)

spawnDetectCmd = "gnome-terminal -e \"hat-detect "
spawnDetectEnd = "\"&"
spawnTraceCmd = "hat-trail "
spawnTraceEnd = "&"


-----------------------------------------------------------------------
-- misc functions
-----------------------------------------------------------------------

stringToInt :: String -> Maybe Int
stringToInt s = stringToInt' True 0 s
 where
  stringToInt' True _ ('#':r) = stringToInt' True 0 r -- skip "#" at beginning
  stringToInt' True _ (' ':r) = stringToInt' True 0 r -- skip " " at beginning
--  stringToInt' False i (' ':r) = Just i
  stringToInt' first i [] = if first then Nothing else  Just i
  stringToInt' _ i (c:r) | (isDigit c) = stringToInt' False (i*10+(digitToInt c)) r
		         | otherwise = Nothing

upStr :: String -> String
upStr = map toUpper

checkParameters :: String -> String -> Int
checkParameters l ('-':r) = checkParameters' l r
    where 
    checkParameters' _ [] = 0
    checkParameters' l (c:r) = if (c `elem` l) then (checkParameters' l r) else 2
checkParameters _ _ = 1

hasBadParameters :: String -> [String] -> Bool
hasBadParameters flags params =
    foldl (\y x->(x==2)||(y)) False (map (checkParameters flags) params)

goodParameters :: String -> [String] -> String
goodParameters flags l = concat (filter (\x->((checkParameters flags x)==0)) l)

noParameters :: [String] -> [String]
noParameters l = filter (\x->((checkParameters [] x)==1)) l

checkIdentifiers (ident1:"in":ident2:file:[]) = Just (ident1,ident2,file)
checkIdentifiers (ident1:file:[]) = Just (ident1,"",file)
checkIdentifiers (file:[]) = Just ("","",file)
checkIdentifiers _ = Nothing

main = 
    do
    arguments <- getArgs
    if ((length arguments)==3)&&((head (tail arguments))=="-remote") then
       startObserve (head arguments) False False False (head (drop 2 arguments)) "" True
      else
         let identifiers = checkIdentifiers (noParameters arguments);
             options = (goodParameters "vxur" arguments) in
          if ((isNothing identifiers)||(hasBadParameters "vxur" arguments)) then
	     showHelp
	   else
            let verboseMode = 'v' `elem` options;
                recursiveMode = 'r' `elem` options;
                expertMode = 'x' `elem` options;
		(ident1,ident2,file) = (fromJust identifiers) in
--             putStrLn (show (goodParameters "vxur" arguments))>>
--             putStrLn (show (noParameters arguments)) >>
--             putStrLn (show ([verboseMode,recursiveMode,expertMode])) >>
	       startObserve file verboseMode recursiveMode expertMode ident1 ident2 False

--startObserve :: String -> Bool -> Bool -> Bool -> String -> String -> Bool -> IO ()
startObserve file verboseMode recursiveMode expertMode ident1 ident2 remote =
    do
      maybehattrace <- openTrace file
      if (isNothing maybehattrace) then
	 do
	   putStrLn ("Hatobserve\n\nError: Unable open file \""++file++"\".")
       else
        let hattrace = (fromJust maybehattrace) in
	  do
            if ((remote)||(ident1=="")) then putStrLn "\nWelcome to Hatobserve (10/7/01)"
             else return ()
            let observed = if ((remote)||(ident1/="")) then
			     makeObserve hattrace verboseMode recursiveMode expertMode
			     False (\ x -> True) ident1 ident2
			    else
			     (Found []) in
              if (ident1=="") then
               do
	 	  dummy <- interactive (file,hattrace) ([],False,1,0,50,
							observableIdents hattrace)
		  return ()
               else
               if (remote) then
                let obs = (fromFound observed);
                    hasmore = (null obs)==False in
                 do
	           dummy <- doCommand "" "" (file,hattrace) ((fromFound observed),
							     hasmore,1,0,50,[])
                   return ()
                else
	        do
      		  if (isTopIdentNotFound observed) then
		     putStrLn ("Sorry, no prize. Nothing recorded in trace about "++
			       "identifier \""++ident2++"\".\n(Check spelling!)")
                    else
                     if (isIdentNotFound observed) then
 		       putStrLn ("Sorry, no prize. Nothing recorded in trace about "++
			         "identifier \""++ident1++"\".\n(Check spelling!)")
                      else
                       printCReductionList 100 (fromFound observed)
                  return ()

 
showObservables :: [HatNode] -> IO ()
showObservables [] = return ()
showObservables (o:obs) =
  do
    putStrLn (hatName o)
    showObservables obs

observableIdents :: HatTrace -> [HatNode]
observableIdents hattrace =
    let r = observables hattrace;
	f = (fromFound r) in
	if (isFound r) then
	    filter (\x -> ((hatNodeType x)==HatIdentNode)) f -- return all identifiers
	 else []

showObservation :: Int -> Int -> HatNode -> IO()
showObservation precision i node =
 do
   putStr ("#"++(show i)++": ")
   printCReduction precision node

showObservationList :: Int -> Int -> Int -> [HatNode] -> IO Int
showObservationList _ _  _ [] = return 0
showObservationList _ i 0 _ = return 0
showObservationList precision i max (e:r) =
    do
      showObservation precision i e
      count <- showObservationList precision (i+1) (max-1) r
      return (count+1)

makeObserve :: HatTrace -> Bool -> Bool -> Bool -> Bool ->
	       (LinExpr->Bool) -> String -> String -> ObserveResult
makeObserve hattrace verboseMode recursiveMode expertMode filterMode 
	    filterFun ident1 ident2 =
    let observed = (observe hattrace ident1 ident2 recursiveMode) in
     if (isFound observed) then
       if (expertMode) then
	   observed
        else
         if (filterMode) then
	    (Found (uniqueFilter (fromFound observed) filterFun))
	  else
              (Found (unique (fromFound observed)))
      else
        observed

unique :: [HatNode] -> [HatNode]
unique observed = unique' observed []
 where
 unique' [] _ = []
 unique' (obs:observed) trie =
    let (b,tries) = (insertTrie trie (linearizeEquation (toHatExpressionTree 200 obs)));
	r = unique' observed tries in
	    if b then (obs:r) else r

uniqueFilter :: [HatNode] -> (LinExpr -> Bool) -> [HatNode]
uniqueFilter observed filterFun = uniqueFilter' observed filterFun []
 where
 uniqueFilter' [] _ _ = []
 uniqueFilter' (obs:observed) filterFun trie =
    let linexpr = linearizeEquation (toHatExpressionTree 200 obs);
	(b,tries) = if (filterFun linexpr) then (insertTrie trie linexpr) else
		    (False,trie);
        r = uniqueFilter' observed filterFun tries in
		if b then (obs:r) else r

last2 :: [a] -> ([a],[a])
last2 [] = ([],[])
last2 (a:b:[]) = ([a,b],[])
last2 (a:list) = let (lst,whole) = last2 list in
  (lst,a:whole)

options :: String -> (String,String)
options s =
  let w = words s;
      dropFun = (\ x->((length x)>0)&&((head x)=='-'));
      o = takeWhile dropFun w;
      r = dropWhile dropFun w in
   ((unwords o),(unwords r))

showSomeMore :: Int -> Int -> Int -> [HatNode] -> IO (Int,Bool)
showSomeMore precision currentEq equationsPerPage observed =
 let showNowList = (drop currentEq observed);
     hasMore = (null (drop equationsPerPage showNowList))==False in
  do
    count <- showObservationList precision (currentEq+1) equationsPerPage
	     showNowList
    return (count+currentEq,hasMore)

interactive :: (String,HatTrace) -> ([HatNode],Bool,Int,Int,Int,[HatNode]) -> IO()
interactive hatfile state@(_,more,_,_,_,_) =
 do
   if (more==False) then
      putStr "\ncommand>: "
    else putStr "\n(<RETURN> for more)>: "
   hFlush stdout
   s <- getLine
   let w = words s;
       cmd = if (null w) then "" else upStr (head w);
    doCommand cmd s hatfile state


getNumberParam :: String -> String -> String -> Maybe Int
getNumberParam cmd pattern1 pattern2 = 
    if (length pattern1)<(length pattern2) then
       getNumberParam' cmd pattern2 pattern1
     else
       getNumberParam' cmd pattern1 pattern2
    where
     getNumberParam' cmd pattern1 pattern2 =
	 let com = upStr cmd in
	   if (take (length pattern1) com)==pattern1 then
	      stringToInt (drop (length pattern1) cmd)
	    else
	    if (take (length pattern2) com)==pattern2 then
               stringToInt (drop (length pattern2) cmd)
	     else
              Nothing


doCommand :: String -> String -> (String,HatTrace) ->
	     ([HatNode],Bool,Int,Int,Int,[HatNode]) -> IO()
doCommand cmd s hatfile state@(lastObserved,more,equationsPerPage,currentPos,
			       precision,observable)
  | (cmd=="")||((length (words s)==1)&&((cmd=="D")||(cmd=="DOWN"))) =
    if (more) then
       do
        (newPos,newMore) <- (showSomeMore precision currentPos equationsPerPage
			     lastObserved)
	interactive hatfile (lastObserved,newMore,equationsPerPage,newPos,precision,
			     observable)
     else
      do
       if (currentPos>0) then putStrLn "No more applications observed." else
	  return () 
       putStrLn "Enter 'h' for help, 'q' to quit."
       interactive hatfile state

-- cmd must be atleast one character long!
doCommand cmd s hatfile@(file,_) state@(lastObserved,more,equationsPerPage,
					currentPos,precision,observable)
    | (cmd=="Q")||(cmd=="QUIT")||(cmd=="EXIT") = putStrLn "Goodbye!\n"
    | (cmd=="H")||(cmd=="HELP") = interactiveHelp >> interactive hatfile state
    | (cmd=="S")||(cmd=="SHOW") =
	(putStrLn "\nObservable Identifier:") >>
	(putStrLn "----------------------") >>
	showObservables observable >>
	interactive hatfile state
    | (cmd=="C")||(cmd=="COUNT") =
        (if (more) then
          putStrLn "One moment, this may take a while..."
         else return ())
        >>
	putStrLn ("Number of unique matching applications: "++
		  (show (length lastObserved))) >>
	interactive hatfile state
    | (isJust (getNumberParam s "L" "LINES")) =
	let newPerPage = fromJust (getNumberParam s "L" "LINES") in
          if (newPerPage>0) then
            putStrLn ("Lines per page set to "++(show newPerPage)) >> 
            interactive hatfile (lastObserved,more,newPerPage,
				 currentPos,precision,observable)
           else
            putStrLn "Lines per page must be greater than 0!" >>
            interactive hatfile state
    | (cmd=="L")||(cmd=="LINES") =
	putStrLn ("Equations per page is currently set to "++(show equationsPerPage)++
		  ".") >> interactive hatfile state
    | (isJust (getNumberParam s "+" "+PREC"))||((cmd=="+")||(cmd=="+PREC")) =
	let val = (getNumberParam s "+" "+PREC");
	    newPrec = precision+(if (isNothing val) then 1 else fromJust val) in
            putStrLn ("Precision set to "++(show newPrec)) >> 
            interactive hatfile (lastObserved,more,equationsPerPage,
				 currentPos,newPrec,observable)
    | (isJust (getNumberParam s "-" "-PREC"))||((cmd=="-")||(cmd=="-PREC")) =
	let val = (getNumberParam s "-" "-PREC");
	    newPrec1 = precision-(if (isNothing val) then 1 else fromJust val);
	    newPrec = if newPrec1<3 then 3 else newPrec1 in
            putStrLn ("Precision set to "++(show newPrec)) >> 
            interactive hatfile (lastObserved,more,equationsPerPage,
				 currentPos,newPrec,observable)
    | (isJust (getNumberParam s "U" "UP")) =
	let up = fromJust (getNumberParam s "U" "UP") in
         if (up==0) then interactive hatfile state else
          let newPos = if up>currentPos then 0 else currentPos-up in
            doCommand "" "" hatfile (lastObserved,
				  if (currentPos==newPos) then more else True,
				  equationsPerPage,newPos,precision,observable)
    | ((isJust (getNumberParam s "D" "DOWN"))||
       (isJust (getNumberParam s "G" "GO"))) =
	let down = if (isJust (getNumberParam s "D" "DOWN")) then
			   (fromJust (getNumberParam s "D" "DOWN"))
		    else
			   (fromJust (getNumberParam s "G" "GO")) in
         if ((head cmd)=='D')&&(down==0) then interactive hatfile state else
          let newPos = if (head cmd)=='D' then currentPos+down else 
		       (if down==0 then 0 else down-1);
              listend = drop newPos lastObserved in
            if (null listend) then  -- check list as far as necessary
	       -- ok, at the list's end: now check for the real end of the list
	       let realPos = length lastObserved in
                putStrLn "Now at the end of the list." >>
                interactive hatfile (lastObserved,False,equationsPerPage,realPos,
				     precision,observable)
            else
             doCommand "" "" hatfile (lastObserved,
				      True,
				      equationsPerPage,newPos,precision,observable)
    | (cmd=="U")||(cmd=="UP") = doCommand "UP" ("UP "++(show (2*equationsPerPage)))
				hatfile state
    | (cmd=="G")||(cmd=="GO") =
	putStrLn ("Go to which number? Use \"go <n>\" to see equation number <n>.")
		  >>
	interactive hatfile state
    | (isJust (stringToInt cmd)) =
        let number = (fromJust (stringToInt cmd));
            node = (drop (number-1) lastObserved) in
         do
          if (number>0) then
            do
--              putStrLn ("starting detect session for #"++
--	 	       (show number)++" in separate window...")
              if (null node) then -- This test may take a while!
	         putStrLn "No equation with this number!"
               else
                 startExternalTool file (head node)
           else
	      putStrLn "No equation with this number!" 
	  interactive hatfile state

doCommand cmd s hatfile state
  | ((cmd=="O")||(cmd=="OBSERVE"))&&((length (words s))==1) =
    do
      putStrLn "\nObserve Wizard"
      putStrLn ""
      putStr "enter function to be observed: "
      hFlush stdout
      fun <- getLine
      putStrLn ""
      if (fun=="") then putStrLn "nothing to be observed" else
       do
        putStrLn "You can observe applications with specific arguments only."
        putStrLn "The '_' matches any subexpression."
        putStrLn "Enter specific arguments separated by spaces, or leave blank."
        putStr "arguments: "
        hFlush stdout
        arguments <- getLine
        putStrLn ""
	putStrLn "You can observe applications which result in a specific result."
        putStrLn "_|_ represents a blackholed result."
        putStr "result (or leave blank): "
        result <- getLine
        putStrLn ""
        putStrLn ("You can observe applications of \""++fun++"\" within the context of ")
        putStrLn "specific second function only."
        putStr "Enter second function (or leave blank): "
        ident2 <- getLine
        putStrLn ""
        rekMode <- if (ident2/="") then return "" else
           do
	     putStr ("Do you want to observe recursive calls of "++fun++"? Y/N:")
             hFlush stdout
	     rek <- getLine
             putStrLn ""
             if ((null rek)==False)&&((toUpper (head rek))=='N') then return "-r " else
		return ""
        let query = "o "++rekMode++
		    (if (null arguments) then fun else ("("++fun++" "++arguments++")"))++
		    (if (null result) then "" else (" = "++result))++
		    (if (null ident2) then "" else (" in "++ident2)) in
	  (putStrLn ("Your query is: "++query++"\n")) >>
          doCommand "O" query hatfile state
        
doCommand cmd s hatfile@(_,hattrace) state@(lastObserved,
					    more,equationsPerPage,currentPos,precision,
					    observable)
  | ((cmd=="O")||(cmd=="OBSERVE"))&&(length (words s)>1) =
   let (opts,p) = (options (unwords (tail (words s))));
       pattern1 = (stringLex p);
       (maybeident,pattern2) = (last2 pattern1);
       (ident2,pattern) =
	   if ((length maybeident)==2)&&((head maybeident)=="in") then
                  (head (tail maybeident),(stringLinExpr pattern2))
		  else ("",(stringLinExpr pattern1));
       fun = (lmoFun pattern)
      in
        do
         putStrLn ("searching for: "++fun++(if ident2/="" then " in "++ident2 else ""))
	 putStrLn ""

         let newObserved = makeObserve hattrace ('v' `elem` opts) ('r' `elem` opts) False
			   ((length pattern)>2)
			   (\x -> (compareExpr x pattern))
			   fun ident2 in
          if (isTopIdentNotFound newObserved) then
            putStrLn ("Sorry, no prize. Nothing recorded in trace about "++
                      "identifier \""++ident2++"\".\n(Check spelling!)") >>
            interactive hatfile state
           else
           if (isIdentNotFound newObserved) then
              putStrLn ("Sorry, no prize. Nothing recorded in trace about "++
                        "identifier \""++fun++"\".\n(Check spelling!)") >>
              interactive hatfile state
            else
--             putStrLn "searching..." >> 
              if (null (fromFound newObserved)) then
		putStrLn "no match found\n" >>
		interactive hatfile state
	      else
               do
                (newPos,newMore) <- (showSomeMore precision 0 equationsPerPage
	 			     (fromFound newObserved))
                interactive hatfile ((fromFound newObserved),newMore,equationsPerPage,
				     newPos,precision,observable)

doCommand cmd s hatfile state =
    if (null cmd) then
       putStrLn "Unknown command. Enter 'h' for help, 'q' to quit." >>
       interactive hatfile state
     else
      doCommand "O" ("O "++s) hatfile state

startExternalTool file node =
    let id = toRemoteRep node;
        rhsID = toRemoteRep (hatResult node) in
    do
      putStr "Start Algorithmic Debugging or Tracer? (A/T): "
      choice <- getLine
      if (((length choice)>0)&&(head(upStr(choice))=='T')) then
        do
          putStr "Trace left-hand-side (lhs) or rhs? (L/R): "
	  lhs <- getLine
          errcode <- system(spawnTraceCmd++
			    file++" -remote "++
			    (if ((rhsID=="")||((length lhs)==0)||
				 (head (upStr lhs)/='R')) then id
			     else rhsID)++
			    spawnTraceEnd)
          if (errcode/=ExitSuccess) then
	     putStrLn ("ERROR: Unable to start hat-trail.\n"++
		       "Check settings and availability of hat-trail!")
	   else return ()
       else
        do
         errcode <- system(spawnDetectCmd++
			   file++" -remote "++id++spawnDetectEnd)
	 if (errcode/=ExitSuccess) then
	    putStrLn ("ERROR: Unable to start hat-detect.\n"++
		      "Check settings and availability of hat-detect!")
	  else return ()

interactiveHelp =
  do
   putStrLn "\n\n\n\n\n\n\n\n\n"
   putStrLn "\nHelp for Interactive Mode"
   putStrLn "---------------------------------------------------------------------------"
   putStrLn " supported commands:"
   putStrLn ""
   putStrLn " o         or observe          make new observation"
   putStrLn " o <query> or observe <query>  make new observation with query"
   putStrLn " s         or show             see a list of observable functions"
   putStrLn ""
   putStrLn " <n>       or #<n>             start algorithmic debugging session"
   putStrLn "                               for observed equation number <n>"
   putStrLn ""
   putStrLn " g <n>     or go <n>           go to observed equation number <n>"
   putStrLn " u <n>     or up <n>           go up in observation list by <n>"
   putStrLn " d <n>     or down <n>         go down in observation list by <n>"
   putStrLn " <RETURN>                      show more applications (if available)"
   putStrLn ""
   putStrLn " l <n>     or lines <n>        set number of equations listed per page to <n>"
   putStrLn " +<n>      or -<n>             increase/decrease precision depth for expressions"
   putStrLn ""
   putStrLn " h         or help             for help"
   putStrLn " q         or quit             quit"
   putStrLn ""
   putStrLn "Press <RETURN> to see help for query-syntax, any other key to return."
   putStrLn ""
   s <- getLine
   if (null s) then queryHelp else return ()

queryHelp =
  do
   putStrLn "\n\n\n\nHelp for Query Syntax"
   putStrLn "---------------------------------------------------------------------------"
   putStrLn "syntax for <query>: \"[-rv] <pattern> [in <identifier>]\""
   putStrLn ""
   putStrLn " option \"-r\" suppresses recursive applications"
   putStrLn " option \"-v\" enables verbose output - to show unevaluated expressions"
   putStrLn ""
   putStrLn " There are three possibilities for <pattern>:"
   putStrLn ""
   putStrLn "  <pattern> can be a simple \"<identifier>\""
   putStrLn "    All applications of <identifier> will be shown."
   putStrLn "  <pattern> can be an application pattern: \"<identfier> [arguments]\""
   putStrLn "    All applications with matching arguments will be shown."
   putStrLn "  <pattern> can be an equation pattern: \"<identifier> [arguments] = <expression>\""
   putStrLn "    All applications of <identifier> with matching arguments resulting in a"
   putStrLn "    <expression> will be shown."
   putStrLn ""
   putStrLn " The underscore \"_\" matches any argument/subexpression!"
   putStrLn ""
   putStrLn "example queries: -r myfunction"
   putStrLn "                 myfunction _ (myConstructor 2 _) in myOtherFunction"
   putStrLn "                 -rv (myfunction \"Hello World!\" (: 1 (: 2 _))) = [1,_]"
   putStrLn ""
   putStrLn "ATTENTION: Currently INFIX operators are not supported in patterns!"
   putStrLn "           Please use the prefix style and enclose applications in parenthesis."

showHelp = 
   do
   putStrLn "\nusage: hat-observe [-v] [-r] [-xu] identifier [in topidentifier] file-name"
   putStrLn "       prints a table of all applications and results of the given"
   putStrLn "       top-level identifier [within the application of topidentifier].\n"
   putStrLn "options:\n"
   putStrLn "       v: verbose mode. Unevaluated expressions are shown in full."
   putStrLn "       r: recursive mode. Omitt recursive function applications.\n"
   putStrLn "       xu: expert's mode for a very fast response. All applications"
   putStrLn "           of the identifier are shown, rather than only the most"
   putStrLn "           general ones.\n"

