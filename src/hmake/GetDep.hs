module GetDep(showdep,showmake,dependency,When) where

import Getmodtime(When(..))
import Imports(getImports)
import FileName
import Unlit(unlit)
import Argv

#ifdef __HBC__
import FileStat
#endif
#if defined(__NHC__) || defined(__GLASGOW_HASKELL__)
import Directory
#endif
import IO
import Time
import List (intersperse)

#if !defined(__HASKELL98__)
#define ioError fail
#endif

{-
showdep (f,(((ths,thi,tobj),p,s,cpp),i)) =
  let cppflag = if cpp then '*' else ' '
  in show f ++ cppflag:'(': show ths ++ ':': show thi ++ ':': show tobj ++ ") "
     ++ show p ++ '(':show s++ ") : " ++ show i ++ "\n"
-}
showdep (f,(((ths,thi,tobj),p,s,cpp),i)) =
  f ++ ": " ++ mix i ++ "\n"
  where mix = foldr (\a b-> a++' ':b) ""

-- showmake opts goaldir (f,(((ths,thi,tobj),p,s,cpp),i)) =
--   dotO f ++ ": " ++ s ++ " " ++ mix i
--   where mix = foldr (\a b-> dotO a ++ ' ':b) "\n"
--         dotO f = fixFile opts goaldir f (oSuffix opts)

showmake opts goaldir ((f,p,s),i) =
  dotO p f ++ ": " ++ s ++ " " ++ mix i
  where mix = foldr (\(a,p) b-> dotO p a ++ ' ':b) "\n"
        dotO p f =
          if (dflag opts) then fixFile opts goaldir f (oSuffix opts)
                          else fixFile opts p       f (oSuffix opts)


dependency opts done [] =
  return done
dependency opts done ((f,dm):fs) =
  if f `elem` (ignoreHi opts) || f `elem` (map fst done)
   then dependency opts done fs
   else readFirst opts f dm >>= \res ->
         case res of 
           Left _ -> dependency opts done fs	-- a Prelude/StdLib file
           Right (t,p,file,source) ->
               readFile source >>= \content->
               let i = filter (`notElem` (ignoreHi opts))
                              (getImports (zdefs opts ++ defs opts) file)
                                    -- previously (haskellImport kp file)
                   hash ('\n':'#':_) = True
                   hash ('\n':'%':_) = True
                   hash (_:xs)  = hash xs
                   hash  []     = False
                   cpp = hash ('\n':content)
                   moredone = (f,((t,p,source,cpp),i)):done
                   newms = map (\x->(x,source)) i
               in
{- Originally, the next line was #ifdef sun, but apparently FreeBSD doesn't
   like too many open files either. -}
                  cpp `seq`
                  dependency opts moredone (newms ++ fs)

--mtime = itos 0 . head . drop (10::Int) . words
-- where
--   itos a (x:xs) | isDigit x = itos (a*10+ord x - ord '0') xs
--   itos a _ = a

#ifdef __HBC__
readTime f = catch (getFileStat f >>= \sf -> return (At ((st_mtime sf)::ClockTime)))
                   (\_ -> return Never)
#endif
#if defined(__NHC__) || defined (__GLASGOW_HASKELL__)
readTime f = --hPutStr stderr ("readTime "++f++"\n") >>
             doesFileExist f >>= \so->
             if so then getModificationTime f >>= \mt -> return (At mt)
             else return Never
#endif


readFirst opts name demand =
  watch ("readFirst " ++ show (pathSrc opts) ++
              "\n   " ++ show (pathPrel opts)) >>
  rN ("":pathSrc opts)
 where
  ff = fixFileName name
  watch = debug opts

#ifdef __HBC__
  rN [] = rP (pathPrel opts) 
  rN (p:ps) =
    let source = fixFile opts p ff "gc"
    in watch ("Trying (N)" ++ source) >>
       catch
         (readFile source >>= \file-> readData p source file)
         (\_-> let source = fixFile opts p ff "hs"
               in watch ("Trying (N)" ++ source) >>
                  catch
                    (readFile source >>= \file -> readData p source file)
                    (\_ -> let source = fixFile opts p ff "lhs"
	                   in watch ("Trying (N)" ++ source) >>
                              catch (readFile source >>= \file ->
                                     readData p source (unlit name file))
                                    (\_ -> rN ps)))

  rP [] = error ("Can't find module "++name++" in\n\t"++
                 concat (intersperse "\n\t" (".":pathSrc opts))++
                 "\n  Or in standard libraries at\n\t"++
                 concat (intersperse "\n\t" (pathPrel opts))++
                 "\n  Asked for by: "++demand++
                 "\n[Check settings of -I or -P flags?]\n")
  rP (p:ps) =
     let source = fixFile opts p ff (hiSuffix opts)
     in watch ("Trying (P)" ++ source) >>
        catch (readFile source >>= \file ->  return (Left file))
              (\_ -> rP ps)
#endif
#if defined(__NHC__) || defined(__GLASGOW_HASKELL__)
  rN [] = rP (pathPrel opts) 
  rN (p:ps) =
    let try  []        = rN ps
        try ((s,f):xs) = watch ("Trying (N)" ++ s) >>
                         doesFileExist s >>= \so->
                         if so then
                             watch ("Got (N)" ++ s) >>
                             readFile s >>= \file-> readData p s (f file)
                         else try xs
    in try [ (fixFile opts p ff "gc",  id),
             (fixFile opts p ff "hs",  id),
             (fixFile opts p ff "lhs", (unlit name))]

  rP [] = error ("Can't find module "++name++" in\n\t"++
                 concat (intersperse "\n\t" (".":pathSrc opts))++
                 "\n  Or in standard libraries at\n\t"++
                 concat (intersperse "\n\t" (pathPrel opts))++
                 "\n  Asked for by: "++demand++
                 "\n[Check settings of -I or -P flags?]\n")
  rP (p:ps) =
     let source = fixFile opts p ff (hiSuffix opts)
     in watch ("Trying (P)" ++ source) >>
        doesFileExist source >>= \so->
        if so then
            watch ("Got (P)" ++ source) >>
            return (Left [])
        else rP ps

#endif

  readData path source file =
     readTime source >>= \ths ->
     readTime (fixFile opts path  ff (hiSuffix opts)) >>= \thi ->
     readTime (fixFile opts opath ff (oSuffix  opts)) >>= \tobj ->
     return (Right  ((ths,thi,tobj),path,file,source))
   where opath = if null (goalDir opts) then path else (goalDir opts)

