module Unlit(unlit) where


--import Prelude((.),not,otherwise,(&&),(||),fromIntegral,($))
--import Prelude(lines,unlines,all,tail,concat,show,error,words,read)
--import PreludeList(map,(++))
import Char

-- Part of the following code is from "Report on the Programming Language Haskell",
-- version 1.2, appendix C.

data Classified = Program String | Blank | Comment | Include Int String | Pre String

classify :: Int -> [String] -> [Classified]
classify _ []                = []
classify _ (('\\':x):xs) | x == "begin{code}" = Blank : allProg xs
   where allProg [] = []  -- Should give an error message, but I have no good position information.
         allProg (('\\':x):xs) |  x == "end{code}" = Blank : classify 0 xs
	 allProg (x:xs) = Program x:allProg xs
classify 0 (('>':x):xs)  = let (sp,code) = span isSpace x in
                           Program code : classify (length sp + 1) xs
classify n (('>':x):xs)  = Program (drop (n-1) x) : classify n xs
classify _ (('#':x):xs)  = (case words x of
                              (line:file:_) | all isDigit line -> Include (read line) file
                              _                                -> Pre x
                           ) : classify 0 xs
classify _ (x:xs) | all isSpace x = Blank:classify 0 xs
classify _ (x:xs)                 = Comment:classify 0 xs

unclassify :: Classified -> String
unclassify (Program s) = s
unclassify (Pre s)     = '#':s
unclassify (Include i f) = '#':' ':show i ++ ' ':f
unclassify Blank       = ""
unclassify Comment     = ""

unlit :: String -> String -> String
unlit file lhs = (unlines . map unclassify . adjecent file (0::Int) Blank . classify 0) (lines lhs)

adjecent :: String -> Int -> Classified -> [Classified] -> [Classified]
adjecent file 0 _             (x              :xs) = x : adjecent file 1 x xs -- force evaluation of line number
adjecent file n y@(Program _) (x@Comment      :xs) = error (message file n "program" "comment")
adjecent file n y@(Program _) (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@(Program _) (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n y@Comment     (x@(Program _)  :xs) = error (message file n "comment" "program")
adjecent file n y@Comment     (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@Comment     (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n y@Blank       (x@(Include i f):xs) = x: adjecent f    i     y xs
adjecent file n y@Blank       (x@(Pre _)      :xs) = x: adjecent file (n+1) y xs
adjecent file n _             (x@next         :xs) = x: adjecent file (n+1) x xs
adjecent file n _             []                    = []

message "\"\"" n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message []     n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message file   n p c = "In file " ++ file ++ " at line "++show n++": "++p++ " line before "++c++" line.\n"



{-
module Unlit(unlit) where
-- This version does not handle \begin{code} & \end{code}, and it is
-- careless with indentation.
unlit = map unlitline

unlitline ('>' : s) = s
unlitline _ = ""
-}

