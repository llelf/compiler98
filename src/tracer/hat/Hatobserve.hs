import HatTrace
import HatTrie
import HatExpressionTree
import PrettyExp(showReduction)
import Maybe
import List(sort)
import Monad(when)
import System
import Char(isDigit,digitToInt,toUpper)
import IO(hFlush,stdout)

spawnDetectCmd = "xterm -e hat-detect "
spawnDetectEnd = "&"
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
--stringToInt' False i (' ':r) = Just i
  stringToInt' first i [] = if first then Nothing else  Just i
  stringToInt' _ i (c:r) | (isDigit c) = stringToInt' False
                                                      (i*10+(digitToInt c))
                                                      r
		         | otherwise = Nothing

upStr :: String -> String
upStr = map toUpper

checkParameters :: String -> String -> Int
checkParameters l ('-':r) =
    checkParameters' l r
  where 
    checkParameters' _ [] = 0
    checkParameters' l (c:r) = if c `elem` l then checkParameters' l r else 2
checkParameters _ _ = 1

hasBadParameters :: String -> [String] -> Bool
hasBadParameters flags params =
    foldl (\y x-> x==2 || y) False (map (checkParameters flags) params)

goodParameters :: String -> [String] -> String
goodParameters flags l = concat (filter (\x-> checkParameters flags x ==0) l)

noParameters :: [String] -> [String]
noParameters l = filter (\x-> checkParameters [] x ==1) l

checkIdentifiers (ident1:"in":ident2:file:[]) = Just (ident1,ident2,file)
checkIdentifiers (ident1:file:[]) = Just (ident1,"",file)
checkIdentifiers (file:[]) = Just ("","",file)
checkIdentifiers _ = Nothing

main = do
    arguments <- getArgs
    putStrLn ("cmdline args: "++show arguments)
    if length arguments ==3  &&  head (tail arguments) == "-remote" then
        startObserve (head arguments) False False False
                     (head (drop 2 arguments)) "" True
      else
         let identifiers = checkIdentifiers (noParameters arguments);
             options = (goodParameters "vxur" arguments) in
         if isNothing identifiers || hasBadParameters "vxur" arguments then do
             putStrLn ("identifiers: "++show identifiers)
             putStrLn ("hasBadParameters: "++show (hasBadParameters "vxur" arguments))
	     putStrLn showHelp
	 else
            let verboseMode = 'v' `elem` options;
                recursiveMode = 'r' `elem` options;
                expertMode = 'x' `elem` options;
		(ident1,ident2,file) = (fromJust identifiers) in
	    startObserve file verboseMode recursiveMode expertMode
                            ident1 ident2 False

--startObserve :: String -> Bool -> Bool -> Bool -> String -> String -> Bool
--                -> IO ()
startObserve file verboseMode recursiveMode expertMode ident1 ident2 remote = do
    maybehattrace <- openTrace file
    when (isNothing maybehattrace)
	 (do putStrLn ("hat-observe: Error: cannot open file \""++file++"\".")
             exitWith (ExitFailure 1))
    let hattrace = fromJust maybehattrace
    when (remote ||  (ident1==""))
         (do putStrLn ("Welcome to hat-observe "++hatVersionNumber)
             putStrLn "Type 'help' for list of commands")
    let observed = if remote || (ident1/="") then
			  makeObserve hattrace recursiveMode
                                      expertMode False False (\x->True)
                                      ident1 ident2
		   else Found []
    if ident1=="" then do
	 dummy <- interactive (file,hattrace)
				(State {lastObserved = []
                                       ,more = False
                                       ,equationsPerPage = 10
                                       ,currentPos = 0
                                       ,precision = 50
                                       ,observable = observableIdents hattrace
				       ,verboseMode = False
                                       ,recursiveMode = False
                                       ,uniqueMode = False})
	 return ()
      else if remote then do
           let obs = fromFound observed
               hasmore = (null obs)==False
	   when (hasmore==False)
                (putStrLn ("\nNo evaluated applications of \""++ident1
			   ++"\" found!\n"))
	   dummy <- doCommand "" "" (file,hattrace)
				(State {lastObserved = fromFound observed
				       ,more = hasmore
                                       ,equationsPerPage = 10
                                       ,currentPos = 0
                                       ,precision = 50
                                       ,observable = observableIdents hattrace
                                       ,verboseMode = False
                                       ,recursiveMode = False
                                       ,uniqueMode = False})
           return ()
      else if isTopIdentNotFound observed then
	   putStrLn ("Sorry, nothing recorded in trace about "
		     ++"identifier \""++ident2++"\".\n(Check spelling!)")
      else if isIdentNotFound observed then
 	   putStrLn ("Sorry, nothing recorded in trace about "
		     ++"identifier \""++ident1++"\".\n(Check spelling!)")
      else do showObservationList verboseMode 20 1 100000 (fromFound observed)
--            printCReductionList 100 (fromFound observed)
              return ()


showObservables :: [HatNode] -> IO ()
showObservables l = showObservables' 0 "" (sort (map hatName l))
 where showObservables' n _ [] = 
	   if ((n-1) `mod` 3 == 2) then
	      return ()
	   else
	      putStrLn ""
       showObservables' n preceeding (o:obs) =
          if (preceeding/=o) then
            do
              putStr (take 26 (" "++o++"                          "))
              if (n `mod` 3 == 2) then putStrLn "" else return ()
	      showObservables' (n+1) o obs
          else 
            showObservables' n o obs

observableIdents :: HatTrace -> [HatNode]
observableIdents hattrace =
    let r = observables hattrace;
	f = (fromFound r) in
    if isFound r then -- return all identifiers
	 filter (\x -> ((hatNodeType x)==HatIdentNode)) f
    else []

showObservation :: Bool -> Int -> Int -> HatNode -> IO ()
showObservation verboseMode precision i node =
  putStr $ showReduction verboseMode precision node ("#"++(show i)++": ") ""
-- do
--   putStr ("#"++(show i)++": ")
--   printReduction verboseMode (toHatLimit precision node)

showObservationList :: Bool -> Int -> Int -> Int -> [HatNode] -> IO Int
showObservationList _ _ _  _ [] = return 0
showObservationList _ _ i 0 _ = return 0
showObservationList verboseMode precision i max (e:r) =
    do
      showObservation verboseMode precision i e
      putStrLn ""
      count <- showObservationList verboseMode precision (i+1) (max-1) r
      return (count+1)

makeObserve :: HatTrace -> Bool -> Bool -> Bool -> Bool
               -> (LinExpr->Bool) -> String -> String -> ObserveResult
makeObserve hattrace recursiveMode expertMode filterMode 
	    uniqueMode filterFun ident1 ident2 =
    let observed = observe hattrace ident1 ident2 recursiveMode in
    if isFound observed then
       if expertMode then
	    observed
       else if filterMode then
	    Found (uniqueFilter uniqueMode (fromFound observed) filterFun)
       else Found (myunique uniqueMode (fromFound observed))
    else observed

myunique :: Bool -> [HatNode] -> [HatNode]
myunique uniqueMode observed = unique' observed []
  where
    unique' [] tries = if uniqueMode then getTrieNodes tries else []
    unique' (obs:observed) trie =
        let (b,tries) = insertTrie trie
                            (linearizeEquation (toHatExpressionTree 200 obs));
	    r = unique' observed tries in
        if uniqueMode then r else if b then (obs:r) else r

uniqueFilter :: Bool -> [HatNode] -> (LinExpr -> Bool) -> [HatNode]
uniqueFilter uniqueMode observed filterFun = uniqueFilter' observed []
  where
    uniqueFilter' [] tries = if uniqueMode then getTrieNodes tries else []
    uniqueFilter' (obs:observed) trie =
        let linexpr = linearizeEquation (toHatExpressionTree 200 obs);
	    (b,tries) = if filterFun linexpr
                        then insertTrie trie linexpr
                        else (False,trie);
            r = uniqueFilter' observed tries in
        if uniqueMode then r else if b then (obs:r) else r

last2 :: [a] -> ([a],[a])
last2 [] = ([],[])
last2 (a:b:[]) = ([a,b],[])
last2 (a:list) = let (lst,whole) = last2 list in (lst,a:whole)

options :: String -> (String,String)
options s =
  let w = words s;
      dropFun = (\ x->((length x)>0)&&((head x)=='-'));
      o = takeWhile dropFun w;
      r = dropWhile dropFun w in
  ((unwords o),(unwords r))

showSomeMore :: InteractiveState -> IO (Int,Bool)
showSomeMore state =
  let showNowList = drop (currentPos state) (lastObserved state);
      hasMore = not (null (drop (equationsPerPage state) showNowList))
  in do
    count <- showObservationList (verboseMode state) (precision state)
                                 (currentPos state + 1)
	                         (equationsPerPage state) showNowList
    return (count + currentPos state, hasMore)

interactive :: (String,HatTrace) -> InteractiveState -> IO()
interactive hatfile state =
 do
   ( if (more state==False) then putStr "\ncommand> "
     else putStr "command (<RETURN> for more equations)> ")
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
	 else if (take (length pattern2) com)==pattern2 then
              stringToInt (drop (length pattern2) cmd)
	 else Nothing

getEquationNumber :: Int -> [HatNode] -> IO (Maybe HatNode)
getEquationNumber number lastObserved =
  let node = (drop (number-1) lastObserved) in
  if (number>0) then
       if (null node) then do -- This test may take a while!
	    putStrLn "No equation with this number!"
	    return Nothing
       else return (Just (head node))
  else return Nothing


data InteractiveState =
    State
	{ lastObserved	:: [HatNode]
	, more		:: Bool
	, equationsPerPage :: Int
	, currentPos	:: Int
	, precision	:: Int
	, observable	:: [HatNode]
	, verboseMode	:: Bool
	, recursiveMode	:: Bool
	, uniqueMode	:: Bool
	}

doCommand :: String -> String -> (String,HatTrace) -> InteractiveState -> IO()
doCommand cmd s hatfile state
  | cmd=="" || (length (words s)==1 && (cmd=="D" || cmd=="DOWN")) =
    if more state then do
        (newPos,newMore) <- showSomeMore state
	interactive hatfile (state {more=newMore,currentPos=newPos})
    else do
        when (currentPos state>0) (putStrLn "No more applications observed.")
        putStrLn "Type 'h' for help, 'q' to quit."
        interactive hatfile state

-- cmd must be at least one character long!
doCommand cmd s hatfile@(file,_) state
    | cmd=="Q" || cmd=="QUIT" || cmd=="EXIT" = putStrLn "Goodbye!\n"
    | cmd=="H" || cmd=="HELP" = do
        interactiveHelp
        interactive hatfile state
    | cmd=="UNIQUE" || cmd=="U" = do
	putStr "unique mode is now "
	if uniqueMode state then putStrLn "OFF" else putStrLn "ACTIVE"
	interactive hatfile (state {uniqueMode=not (uniqueMode state)})
    | cmd=="V" || cmd=="VERBOSE" = do
	putStr "verbose mode is now "
	if verboseMode state then putStrLn "OFF" else putStrLn "ACTIVE"
	interactive hatfile (state {verboseMode=not (verboseMode state)})
    | cmd=="R" || cmd=="RECURSIVE" = do
	putStr "recursive-call-filter is now "
	if recursiveMode state then putStrLn "OFF" else putStrLn "ACTIVE"
	interactive hatfile (state {recursiveMode=not (recursiveMode state)})
    | cmd=="S" || cmd=="SHOW" = do
	putStrLn "\nObservable Identifiers:"
	putStrLn (take 79 (repeat '-'))
	showObservables (observable state)
	interactive hatfile state
    | cmd=="C" || cmd=="COUNT" = do
        when (more state)
             (putStrLn "One moment, this may take a while...")
	putStrLn ("Number of unique matching applications: "
		  ++(show (length (lastObserved state))))
	interactive hatfile state
    | isJust (getNumberParam s "L" "LINES") =
	let newPerPage = fromJust (getNumberParam s "L" "LINES") in
        if (newPerPage>0) then do
            putStrLn ("Lines per page set to "++(show newPerPage))
            interactive hatfile (state {equationsPerPage=newPerPage})
        else do
            putStrLn "Lines per page must be greater than 0!"
            interactive hatfile state
    | cmd=="L" || cmd=="LINES" = do
	putStrLn ("Equations per page is currently set to "
                  ++(show (equationsPerPage state))++".")
        interactive hatfile state
    | isJust (getNumberParam s "+" "+PREC") || cmd=="+" || cmd=="+PREC" =
	let val = (getNumberParam s "+" "+PREC");
	    newPrec = precision state +
                      (if (isNothing val) then 1 else fromJust val) in do
        putStrLn ("Precision set to "++(show newPrec))
        interactive hatfile (state {precision=newPrec})
    | isJust (getNumberParam s "-" "-PREC") || cmd=="-" || cmd=="-PREC" =
	let val = (getNumberParam s "-" "-PREC");
	    newPrec1 = precision state -
                       (if (isNothing val) then 1 else fromJust val);
	    newPrec = if newPrec1 < 3 then 3 else newPrec1 in do
        putStrLn ("Precision set to "++(show newPrec))
        interactive hatfile (state {precision=newPrec})
    | isJust (getNumberParam s "U" "UP") =
	let up = fromJust (getNumberParam s "U" "UP") in
        if (up==0) then interactive hatfile state
        else
          let newPos = if up > currentPos state then 0
                                                else currentPos state - up in
          doCommand "" "" hatfile (state {more=(if currentPos state==newPos
                                                then more state else True)
                                         ,currentPos=newPos})
    | isJust (getNumberParam s "D" "DOWN") ||
      isJust (getNumberParam s "G" "GO") =
	let down = if (isJust (getNumberParam s "D" "DOWN")) then
		        (fromJust (getNumberParam s "D" "DOWN"))
		   else (fromJust (getNumberParam s "G" "GO"))
        in
        if ((head cmd)=='D')&&(down==0) then interactive hatfile state
        else
          let newPos = if (head cmd)=='D' then currentPos state + down else 
		       (if down==0 then 0 else down-1);
              listend = drop newPos (lastObserved state) in
          if null listend then  -- check list as far as necessary
	       -- ok, at the list's end: now check for the real end of the list
	       let realPos = length (lastObserved state) in do
               putStrLn "Now at the end of the list."
               interactive hatfile (state {more=False,currentPos=realPos})
          else doCommand "" "" hatfile (state {more=True,currentPos=newPos})
    | cmd=="U" || cmd=="UP" =
        doCommand "UP" ("UP "++(show (2*equationsPerPage state))) hatfile state
    | cmd=="G" || cmd=="GO" = do
	putStrLn ("To which number? Use \"go <n>\" to see equation number <n>.")
	interactive hatfile state   
    | cmd=="TRAIL" || cmd=="TRACE" || cmd=="T" = do
	let trail = getNumberParam s "T" "TRAIL";
            trail2 = getNumberParam s "T" "TRACE";
	    number = if isJust trail then fromJust trail else fromJust trail2
        if isJust trail || isJust trail2 then do
               node <- getEquationNumber number (lastObserved state)
	       if isJust node then
                    startExternalTool "T" file number (fromJust node)
	         else return ()
          else putStrLn "No equation number specified!"
	interactive hatfile state
    | cmd=="DEBUG" || cmd=="D" = do
	let debug = getNumberParam s "D" "DEBUG";
	    number = fromJust debug
        if isJust debug then do
              node <- getEquationNumber number (lastObserved state)
	      if isJust node then
                   startExternalTool "A" file number (fromJust node)
	        else return ()
          else putStrLn "No equation number specified!"
	interactive hatfile state

    | isJust (stringToInt cmd) = do
        let number = (fromJust (stringToInt cmd));
            node = (drop (number-1) (lastObserved state))
        if number>0 then do
--             putStrLn ("starting detect session for #"
--	 	         ++(show number)++" in separate window...")
               if null node then -- This test may take a while!
	            putStrLn "No equation with this number!"
                 else startExternalTool "" file number (head node)
          else putStrLn "No equation with this number!" 
	interactive hatfile state

doCommand cmd s hatfile state
  | (cmd=="O" || cmd=="OBSERVE") && length (words s) == 1   = do
    putStrLn "\nObserve Wizard"
    putStrLn ""
    putStr "enter function to be observed: "
    hFlush stdout
    fun <- getLine
    putStrLn ""
    if (fun=="") then do
        putStrLn "nothing to be observed"
        interactive hatfile state         
      else do
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
        putStrLn ("You can observe applications of \""++fun
                  ++"\" within the context of ")
        putStrLn "specific second function only."
        putStr "Enter second function (or leave blank): "
        ident2 <- getLine
        putStrLn ""
        rekMode <-
          if (ident2/="") then return ""
          else do
	    putStr ("Do you want to observe recursive calls of "++fun++"? Y/N:")
            hFlush stdout
	    rek <- getLine
            putStrLn ""
            if ((null rek)==False)&&((toUpper (head rek))=='N') then
                return "-r "
              else
	        return ""
        let query = "o "++rekMode++
		    (if null arguments then fun
                                       else ("("++fun++" "++arguments++")"))++
		    (if null result then "" else (" = "++result))++
		    (if null ident2 then "" else (" in "++ident2))
        putStrLn ("Your query is: "++query++"\n")
        doCommand "O" query hatfile state
        
doCommand cmd s hatfile@(_,hattrace) state
  | (cmd=="O" || cmd=="OBSERVE") && length (words s) > 1  =
    let (opts,p) = options (unwords (tail (words s)))
        pattern1 = stringLex p
        (maybeident,pattern2) = last2 pattern1
        (ident2,(pattern,patternError)) =
	   if length maybeident == 2  &&  head maybeident == "in" then
                  (head (tail maybeident),(stringLinExpr pattern2))
	   else ("",(stringLinExpr pattern1))
        fun = lmoFun pattern
    in if not (null patternError) then do
         putStrLn ("ERROR in pattern: "++patternError)
         interactive hatfile state
       else do
      -- putStrLn ("pattern: "++(show pattern))
         putStrLn ("searching for: "++fun
                   ++(if ident2/="" then " in "++ident2 else ""))

         let newObserved = makeObserve hattrace
			       (recursiveMode state || ('r' `elem` opts))
                               False
			       ((length pattern)>2)
                               (uniqueMode state)
			       (\x -> (compareExpr x pattern))
			       fun ident2
         if isTopIdentNotFound newObserved then do
            putStrLn ("Sorry, nothing recorded in trace about "
                      ++"identifier \""++ident2++"\".\n(Check spelling!)")
            interactive hatfile state
           else if isIdentNotFound newObserved then do
            putStrLn ("Sorry, nothing recorded in trace about "
                      ++"identifier \""++fun++"\".\n(Check spelling!)")
            interactive hatfile state
           else if null (fromFound newObserved) then do
--              putStrLn "searching..."
		putStrLn "no match found\n"
		interactive hatfile state
	   else do
            (newPos,newMore) <- showSomeMore
                                  (state { currentPos=0
	 			         , lastObserved=fromFound newObserved})
            interactive hatfile (state {lastObserved=fromFound newObserved
                                       ,more=newMore
                                       ,currentPos=newPos})

doCommand cmd s hatfile state =
    if null cmd then do
         putStrLn "Unknown command. Type 'h' for help, 'q' to quit."
         interactive hatfile state
    else doCommand "O" ("O "++s) hatfile state

startExternalTool tool file number node = do
    let id = toRemoteRep node;
        rhsID = toRemoteRep (hatResult node)
    choice <- if tool=="" then do
		  putStr "Start Algorithmic Debugging or Tracer? (A/T): "
		  choice <- getLine
		  return choice
              else return tool
    if length choice >0  &&  head (upStr (choice)) =='T' then do
          putStr ("Equation "++(show number)
                  ++": Trace left-hand-side (lhs) or rhs of equation? (L/R): ")
	  lhs <- getLine
          errcode <- system (spawnTraceCmd++file++" -remote "
			     ++(if ((rhsID=="")||((length lhs)==0)||
				   (head (upStr lhs)/='R')) then id
			        else rhsID)
			    ++spawnTraceEnd)
          when (errcode/=ExitSuccess)
	       (putStrLn ("ERROR: Unable to start hat-trail.\n"
		          ++"Check settings and availability of hat-trail!"))
      else do
         errcode <- system (spawnDetectCmd++file++" -remote "
                            ++id++spawnDetectEnd)
	 when (errcode/=ExitSuccess)
	      (putStrLn ("ERROR: Unable to start hat-detect.\n"
		         ++"Check settings and availability of hat-detect!"))

interactiveHelp = do
    putStrLn helptext
    s <- getLine
    when (null s) (putStrLn queryHelp)
  where
    helptext = "\n\
\In interactive mode, commands are:\n\
\ command           abbrev.     meaning\n\
\---------------------------------------------------------------------------\n\
\ observe           o           make new observation\n\
\ observe <query>   o <query>   make new observation with query\n\
\ show              s           see a list of observable functions\n\
\ debug <n>                     start Algorithmic Debugging session\n\
\                               for observed equation number <n>\n\
\ trail <n>                     start Redex-Trail browser for\n\
\                               observed equation number <n>\n\
\ go <n>            g <n>       go to observed equation number <n>\n\
\ up <n>                        go up in observation list by <n>\n\
\ down <n>          d <n>       go down in observation list by <n>\n\
\ <RETURN>                      show more applications (if available)\n\
\ lines <n>         l <n>       set number of equations listed per page to <n>\n\
\ +<n> or -<n>                  increase/decrease precision depth for expressions\n\
\ verbose           v           to toggle verbose mode\n\
\                                ON:  unevaluated expressions are shown in full\n\
\                                OFF: unevaluated expressions are shown as an \"_\"\n\
\ recursive         r           to toggle recursive filter mode\n\
\                                ON:  recursive calls to functions are omitted\n\
\                                OFF: all calls to functions are observed\n\
\ unique            u           to toggle unique mode\n\
\                                ON:  most general equations shown only\n\
\                                     (slow but effective!)\n\
\                                OFF: ditto but less effective and fast\n\
\ help              h           for help\n\
\ quit              q           quit\n\
\\n\
\Press <RETURN> for query syntax, any other key to quit help."

    queryHelp = "\
\---------------------------------------------------------------------------\n\
\An 'observe' query requires a function identifier, and can optionally\n\
\pattern-match on: specific arguments to the function;, a specific\n\
\ result; a specific caller function.  The full query syntax is:\n\
\    <identifier> [<pattern>]*  ['=' <pattern>]?  ['in' <identifier>]?\n\
\where\n\
\    <pattern> = _                                   wildcard\n\
\              | ''' <char> '''                      character\n\
\              | '\"' <string> '\"'                    string\n\
\              | <num>                               number\n\
\              | '[' <pattern> [',' <pattern>]* ']'  literal list\n\
\              | <Constr>                            nullary constructor\n\
\              | '(' <Constr> [<pattern>]* ')'       constructor application\n\
\Note: Currently INFIX operators are not supported in patterns.\n\
\      Please use the prefix style and enclose applications in parenthesis.\n\
\Examples:\n\
\    myfunction\n\
\    myfunction _ (MyConstructor 2 _) in myOtherFunction\n\
\    (myfunction \"Hello World!\" (: 1 (: 2 _))) = [1,_]"

showHelp = "\
\Usage:   hat-observe [-v] [-r] [-xu] identifier [in topidentifier] filename\n\
\Description:\n\
\       prints a table of all applications and results of the given\n\
\       top-level identifier [within the application of topidentifier].\n\
\Options:\n\
\       v: verbose mode. Unevaluated expressions are shown in full.\n\
\       r: recursive mode.  Omit recursive function applications.\n\
\       xu: expert's mode for a very fast response. All applications\n\
\           of the identifier are shown, rather than only the most\n\
\           general ones."

