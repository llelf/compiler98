import HatTrace
import HatTrie
import HatExpression
import Maybe
import System
import Char(isDigit,digitToInt,toUpper)
import IO(hFlush,stdout)

spawnObserveCmd = "gnome-terminal -e \"hat-observe "
spawnObserveEnd = "\"&"
spawnTraceCmd = "hat-trail "
spawnTraceEnd = "&"


-----------------------------------------------------------------------
-- misc functions
-----------------------------------------------------------------------

stringToInt :: String -> Maybe Int
stringToInt s = stringToInt' True 0 s
 where
  stringToInt' True _ (c:r) | (isDigit c)==False = stringToInt' True 0 r -- skip alphas at beginning
--  stringToInt' False i (' ':r) = Just i
  stringToInt' first i [] = if first then Nothing else  Just i
  stringToInt' _ i (c:r) | (isDigit c) = stringToInt' False (i*10+(digitToInt c)) r
		         | otherwise = Nothing

checkArguments :: [String] -> Bool
checkArguments arguments =
    let l = length arguments in
	    if (l==1) then True else
	       if (l/=3) then False else
		  ((head (tail arguments))=="-remote")

getStartReduction :: HatTrace -> [String] -> HatNode
getStartReduction hattrace arguments =
    if ((length arguments)==3) then
       let adr = remoteToNode hattrace (head (drop 2 arguments)) in
	   if (isValidNode adr) then adr else
	      (getStartReduction hattrace [])
     else
       let maybeMainCAF = hatMain hattrace in
         if (isInvalidNode maybeMainCAF) then
	    error "Bad file format! \"main\" CAF could not be found!"
          else maybeMainCAF

upStr :: String -> String
upStr = map toUpper

main = do
         arguments <- getArgs
         if (checkArguments arguments)==False then
	    showHelp
	  else
	  let file = (head arguments) in -- first argument is file name
	   do
             maybehattrace <- openTrace file -- open Redex Trail file
	     if (isNothing maybehattrace) then
		putStrLn ("hat-detect\n\nError: Unable open file \""++file++"\".")
	      else
	       let hattrace = (fromJust maybehattrace);
		   startReduction = (getStartReduction hattrace arguments) in
		 do
	          putStrLn "\nWelcome to Hatdetect (25/08/01)"
                  (b,e,newstate) <- interactive (file,hattrace) ([startReduction],[],
								 [],[],
								 1,50,False,True,False)
		  if (b&&(e>(-1))) then putStrLn "Ok, no error found." else return ()
		  return ()

type StateType = ([HatNode],[(HatNode,LinExpr,Bool)], -- type for state of session
		  [HatNode],[(Int,HatNode)],
		  Int,Int,Bool,Bool,Bool)

showIdent :: HatNode -> String
showIdent node = hatCExpressionStr node 0 10

showRed :: Bool -> Int -> HatNode -> String
showRed verboseMode precision node =
    let v = if verboseMode then 1 else 0;
	s1 = (hatCExpressionStr node v precision);
	res = (hatResult node);
	s2 = if (isValidNode res) then
                (" = "++(hatCExpressionStr res v precision))
	      else
	        ""
	in
	  s1++s2

-- add new node to the list of recent nodes. List holds node, linear representaion of it,
-- and the value of the users answer (Yes=True, No=False)
addToRecentNodes :: [(HatNode,LinExpr,Bool)] -> HatNode -> Bool -> [(HatNode,LinExpr,Bool)]
addToRecentNodes recentNodes node answerYes =
    (node,linearizeExpr (lazyExpression 100 node),answerYes):recentNodes

-- check, whether node is less general than an earlier given answer
memoizeCheck :: [(HatNode,LinExpr,Bool)] -> HatNode -> Maybe Bool
memoizeCheck recentNodes node = (memoizeCheck' recentNodes
				 (linearizeExpr (lazyExpression 100 node)))
  where
   memoizeCheck' [] _ = Nothing
   memoizeCheck' ((_,expr2,answer):recentNodes) expr1 =
       if (compareExpr expr1 expr2) then (Just answer) else
	  memoizeCheck' recentNodes expr1

interactive :: (String,HatTrace) -> StateType -> IO(Bool,Int,StateType)
interactive _ state@([],_,_,[],_,_,_,_,_) = return (True,0,state) -- nothing else to do
interactive hatfile state@([],recentNodes,trusted,((qn,node):postponed),questnumber,
			   precision,verboseMode,memoMode,reconsider) =
 interactive hatfile ([node],  -- take one postponed question as new current question
		      recentNodes,trusted,postponed,qn,precision,
		      verboseMode,memoMode,False)
interactive hatfile state@((node:children),recentNodes,trusted,postponed,questnumber,
			   precision,verboseMode,memoMode,reconsider) =
  -- ask about correctness of "node"
  let answer = (memoizeCheck recentNodes node) in
   if ((node `elem` (map (\(a,_,_)->a) recentNodes))|| -- don't ask about identical(!) node
      ((memoMode)&&((isJust answer)))) then -- in memoizemode
     if (fromJust answer) then
       doCommand "YES" "YES" hatfile state -- pretend user answered "YES"
      else
       doCommand "NO" "NO" hatfile state   -- pretend user answered "NO"
   else
   if ((hatLeftmost node) `elem` trusted) then -- 
     doCommand "YES" "YES" hatfile state   -- pretend user answered "YES"
--     interactive hatfile (children,
--			  (addToRecentNodes recentNodes node True),
--			  trusted,postponed,questnumber+1,
--			  precision,verboseMode,memoMode,False)
    else
     do
       putStrLn ""
       if (reconsider) then putStrLn "reconsider: " else return ()
       putStr ((show (questnumber))++"> ")
       putStr (showRed verboseMode precision node)
       if (reconsider) then putStr "  (Y/?Y/N): " else putStr "  (Y/?Y/?N/N): "
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

doCommand :: String -> String -> (String,HatTrace) -> StateType -> IO(Bool,Int,StateType)
doCommand cmd s hatfile@(file,_) state@((child:children),recentNodes,trusted,postponed,
					questnumber,precision,verboseMode,memoMode,
					reconsider)
    | (cmd=="Q")||(cmd=="QUIT")||(cmd=="EXIT") =
	(putStrLn "Goodbye!\n") >> return (True,-1,state)
    | (cmd=="H")||(cmd=="HELP") = interactiveHelp True >> interactive hatfile state

    -- toggle modes: verbose and memorize
    | (cmd=="V")||(cmd=="VERBOSE") = 
	putStrLn ("   verbose mode is now "++
		  if verboseMode then "OFF" else "ON")
        >>
        interactive hatfile ((child:children),recentNodes,trusted,postponed,
			     questnumber,precision,(not verboseMode),memoMode,reconsider)
    | (cmd=="M")||(cmd=="MEMORIZE") = 
	putStrLn ("   memorize mode is now "++
		  if memoMode then "OFF" else "ON")
        >>
        interactive hatfile ((child:children),recentNodes,trusted,postponed,
			     questnumber,precision,verboseMode,(not memoMode),reconsider)

    -- user defined function trusting
    | (cmd=="T")||(cmd=="TRUST") =
	let trustFun = (hatLeftmost child) in
          do
	    putStrLn ("   Ok, \""++(showIdent trustFun)++
		      "\" will be trusted from now on.")
            (b,q,newstate) <- (interactive hatfile
			       (children,
				(addToRecentNodes recentNodes child True),
				(trustFun:trusted),postponed,
				questnumber+1,precision,
				verboseMode,memoMode,False))
	    if (q==questnumber) then
	       let (_,_,_,_,_,newprec,nverbose,nmemo,_) = newstate in
                 interactive hatfile ((child:children),recentNodes,trusted,postponed,
				      questnumber,newprec,nverbose,nmemo,False)
             else
               return (b,q,newstate)
    | (cmd=="U")||(cmd=="UNTRUST") =
        do
	  putStrLn "Ok, all functions are untrusted now."
	  interactive hatfile (child:children,recentNodes,[],postponed,
			       questnumber,precision,verboseMode,memoMode,reconsider)

    -- answering the question: yes, no, ?yes and ?no
    | (cmd=="Y")||(cmd=="YES") =
	do
          (b,q,newstate) <- (interactive hatfile
			     (children,
			      (addToRecentNodes recentNodes child True),
			      trusted,postponed,
			      questnumber+1,precision,
			      verboseMode,memoMode,False))
	  if (q==questnumber) then
	     let (_,_,_,_,_,newprec,nverbose,nmemo,_) = newstate in
	       interactive hatfile ((child:children),recentNodes,trusted,postponed,
				    questnumber,newprec,nverbose,nmemo,False) 
           else
             return (b,q,newstate)
    | (cmd=="?Y")||(cmd=="?YES")||(cmd=="Y?")||(cmd=="YES?") =
	do
          (b,q,newstate) <- interactive hatfile (children,recentNodes,
						 trusted,
						 (postponed++[(questnumber,child)]),
						 questnumber+1,precision,
						 verboseMode,memoMode,False)
	  if (q==questnumber) then
	     let (_,_,_,_,_,newprec,nverbose,nmemo,_) = newstate in
	       interactive hatfile ((child:children),recentNodes,trusted,postponed,
				    questnumber,newprec,nverbose,nmemo,False) 
           else
             return (b,q,newstate)
    | (cmd=="N")||(cmd=="NO")||
      ((not reconsider)&&((cmd=="?N")||(cmd=="?NO")||(cmd=="N?")||(cmd=="NO?"))) =
	let newchildren = detect child in
	    do
              (b,q,newstate) <- interactive hatfile 
				(newchildren,
				 (addToRecentNodes recentNodes child False),
				 trusted,[],
				 questnumber+1,precision,
				 verboseMode,memoMode,False)
              let (_,_,ntrusted,_,_,newprec,nverbose,nmemo,_) = newstate in
               if (q==questnumber) then
              	  interactive hatfile ((child:children),recentNodes,trusted,postponed,
				       questnumber,newprec,nverbose,nmemo,False)
                else
                 if (b&&(q==0)) then
                  if ('?' `elem` cmd) then
                     interactive hatfile ((child:children),recentNodes,ntrusted,
					  postponed,
					  questnumber,newprec,nverbose,nmemo,True)
                   else
                    let lmo = hatLeftmost child;
			(row,column,modname,modsrcpos,_) =
			    if (isInvalidNode lmo) then (0,0,"","",False) else
			       (fromJust (hatSourceRef lmo)) in
	             do
                     putStrLn "\nErroneous reduction: "
		     putStrLn (showRed verboseMode precision child)
		     putStrLn ("\nBug found within the body of function: \""++
			       (showIdent lmo)++"\"")
		     putStrLn ("line "++(show row)++", column "++(show column)++
			       " in module \""++modname++"\", file: \""++modsrcpos++
			       "\"\n")
		     putStr ("Press 'q' to quit, any other key to go back to question "++
			     (show questnumber)++": ")
		     s <- getLine
		     if ((upStr s) `elem` ["Q","QUIT"]) then
                       return (False,-1,newstate)
		      else
                       interactive hatfile state
		   else
                     return (b,q,newstate)
    | (cmd=="?N")||(cmd=="?NO")||(cmd=="N?")||(cmd=="NO?") =
	putStrLn ("The question is already to be reconsidered. "++
		  "Answer \"Y\", \"N\" or \"?Y\"") >>
        interactive hatfile state
    | (isJust (getNumberParam s "G" "GO")) =     -- handle "go" command
        let number = (fromJust (getNumberParam s "G" "GO")) in
         do
          if (number>0)&&(number<questnumber) then
	     return (False,number,state)  -- return "number", and ask question again
           else
	      (putStrLn "No question with this number!")>>
	      interactive hatfile state
    | (cmd=="O")||(cmd=="OBSERVE") =     -- handle "observe" command
	let obsFun = (hatLeftmost child) in
         do
          errcode <- system(spawnObserveCmd++
			    file++" -remote "++
			    (show (showIdent obsFun))++
			    spawnObserveEnd)
     	  if (errcode/=ExitSuccess) then
	     putStrLn ("ERROR: Unable to start hat-observe.\n"++
		       "Check settings and availability of hat-observe!")
	     else return ()
	  interactive hatfile state
    | (cmd=="R")||(cmd=="REDEX") =     -- handle "redex" command
	let lhsID = nodeToRemote child;
	    rhsID = nodeToRemote (hatResult child) in
	do
          putStr "Trace left-hand-side (lhs) or rhs? (L/R): "
	  lhs <- getLine
          errcode <- system(spawnTraceCmd++
			    file++" -remote "++
			    (if ((rhsID=="")||((length lhs)==0)||
				 (head (upStr lhs)/='R')) then lhsID
			     else rhsID)++
			    spawnTraceEnd)
          if (errcode/=ExitSuccess) then
	     putStrLn ("ERROR: Unable to start hat-trail.\n"++
		       "Check settings and availability of hat-trail!")
	   else return ()
          interactive hatfile state

doCommand cmd s hatfile state =
    putStrLn "Unknown command. Enter 'h' for help, 'q' to quit." >>
    interactive hatfile state


interactiveHelp reconsider =
  do
   putStrLn "\n\n\n\n\n\n\n\n\n"
   putStrLn "\nHelp for Interactive Mode"
   putStrLn "---------------------------------------------------------------------------"
   putStrLn " supported commands:"
   putStrLn ""
   putStrLn " y   or  yes      if the equation is ok."
   putStrLn " n   or  no       if it isn't."
   if (reconsider) then
      putStrLn " ?n  or  ?no      might be wrong, but you are unsure."
    else return ()
   putStrLn " ?y  or  ?yes     to postpone the question."
   putStrLn "\n t   or  trust    if the function shall now be trusted."
   putStrLn " u   or  untrust  to untrust all functions again."
   putStrLn "\n m   or  memorize to toggle the memorize mode."
   putStrLn " c   or  clear    to clear all memorized answers."
   putStrLn " v   or  verbose  to toggle verbose mode.\n"
   putStrLn " g n or  go n     to go back to question <n>.\n"
   putStrLn " o   or  observe   to observe all applications of the current function."
   putStrLn " r   or  redex    to start the redex trail browser.\n"
   putStrLn "\n +[n]  or -[n]    to increase or decrease the output precision [by n]."
   putStrLn "\n q   or  quit     to leave the tool."
   putStrLn "\nAny key to continue..."
   s <- getLine
   return ()

showHelp = 
   do
   putStrLn "\nusage: hat-detect file-name"
   putStrLn "         algorithmic debugging on a hat redex trace file\n\n"

