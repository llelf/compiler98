import HatTrace
import HatTrie
import HatExpression
import Maybe
import System

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

main = do
         arguments <- getArgs
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
	     do
             hattrace <- openTrace file
	     if (ident1=="") then
               interactive hattrace
              else
               makeObserve hattrace verboseMode recursiveMode expertMode
			   False (\ (x,y) -> True) ident1 ident2

makeObserve hattrace verboseMode recursiveMode expertMode filterMode filterFun ident1 ident2 =
    let recmod = (if recursiveMode then 1 else 0);
	observed = (observe hattrace ident1 ident2 recmod) in
     if (expertMode) then
--        showReductionList (map lazyExpression (observe hattrace ident1 ident2
--					       recmod))
	old_showReductionList observed
       else
        if (filterMode) then uniqueFilter observed filterFun else unique observed

unique observed =
    let tries = (insertTrieList []
		 (map (\x -> let l = lazyExpression x in (linearizeExpr l,
							  linearizeExpr (res l)))
		  observed)) in
      let nodes = (getTrieNodes tries) in
       if (null nodes) then putStrLn "no match" else old_showReductionList nodes

uniqueFilter observed filterFun =
    let linexpr = (map (\x -> let l = lazyExpression x in (linearizeExpr l,
							   linearizeExpr (res l)))
		   observed);
        tries = (insertTrieList []
		 (filter filterFun linexpr)) in
--    (putStrLn (showLinList (head linexpr)))>>
      let nodes = (getTrieNodes tries) in
       if (null nodes) then putStrLn "no match" else old_showReductionList nodes

last2 :: [a] -> ([a],[a])
last2 [] = ([],[])
last2 (a:b:[]) = ([a,b],[])
last2 (a:list) = let (lst,whole) = last2 list in
  (lst,a:whole)

options :: String -> (String,String)
options s =
  let w = words s;
      dropFun = (\x->((length x)>0)&&((head x)=='-'));
      o = takeWhile dropFun w;
      r = dropWhile dropFun w in
   ((unwords o),(unwords r))

interactive hattrace =
-- let s = "funny _" in
 do
   putStr "\nEnter search pattern: "
   s <- getLine
   let (opts,p) = (options s);
       pattern1 = (stringLex p);
       (maybeident,pattern2) = (last2 pattern1);
       (ident2,(patternL,patternR)) =
	   if ((length maybeident)==2)&&((head maybeident)=="in") then
                  (head (tail maybeident),(stringLinExpr pattern2))
		  else ("",(stringLinExpr pattern1));
       fun = (lmoFun patternL)
      in
    do
--      putStrLn ((showLinList patternL)++"\n"++(showLinList patternR))
      if ((fun=="EXIT")||(fun=="exit")||(fun=="q")||(fun=="quit")) then
         putStrLn "Goodbye!" else
       if (fun=="") then (putStrLn "No function given to be observed!") >>
          interactiveHelp >>
	  interactive hattrace
        else
        do
         putStrLn ("searching for: "++fun++(if ident2/="" then " in "++ident2 else ""))
	 putStrLn ""
	 makeObserve hattrace ('v' `elem` opts) ('r' `elem` opts) False
			   (((length patternL)>2)||(length patternR>0))
			   (if (null patternR) then (\ (x,_)-> (compareExpr x patternL))
			    else (\ (x,xres) -> ((compareExpr x patternL)&&
						 (compareExpr xres patternR))))
			   fun ident2
         interactive hattrace

interactiveHelp =
  do
   putStrLn "\nhelp for interactive mode:\n"
   putStrLn "usage: \"[-rv] <pattern> [in <identifier>]\""
   putStrLn ""
   putStrLn "option \"-r\" suppresses recursive applications"
   putStrLn "option \"-v\" enables verbose output - to show unevaluated expressions"
   putStrLn ""
   putStrLn "There are three possibilities for pattern:"
   putStrLn " <pattern> can be a simple \"<identifier>\""
   putStrLn "   All applications of <identifier> will be shown."
   putStrLn " <pattern> can be an application pattern: \"<identfier> [arguments]\""
   putStrLn "   All applications with matching arguments will be shown."
   putStrLn " <pattern> can be an equation pattern: \"<identifier> [arguments] = <expression>\""
   putStrLn "   All applications of <identifier> with matching arguments resulting in a <expression> will be shown."
   putStrLn "The underscore \"_\" matches any argument!"
   putStrLn ""
   putStrLn "examples: \"-r f\""
   putStrLn "          \"f _ 3 in g\""
   putStrLn "          \"f 1 _ = [1,_]\""
   putStrLn ""
   putStrLn "enter \"q\" or \"quit\" to quit."

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
   putStrLn "           general ones. Using this option may result in incomplete"
   putStrLn "           applications, missing arguments. \n"

