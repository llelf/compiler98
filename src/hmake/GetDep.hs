module GetDep(showdep,showmake,dependency,When) where
import Getmodtime(When(..))
--import Env(haskellImport)
import Imports(getImports)
import FileName
--import Either
import Unlit(unlit)
#ifdef __HBC__
import FileStat
#endif
#if defined(__NHC__) || defined(__GLASGOW_HASKELL__)
import Directory
--import Cpp
#endif
import IO

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

showmake unixf goalDir (f,(((ths,thi,tobj),p,s,cpp),i)) =
  dotO f ++ ": " ++ s ++ " " ++ mix i
  where mix = foldr (\a b-> dotO a ++ ' ':b) "\n"
        dotO f = fixFile unixf goalDir f "o"

dependency unixf defs kp op ihi ds srcPath prelPath [] =
  return ds
dependency unixf defs kp op ihi ds srcPath prelPath (f:fs) =
  if f `elem` ihi || f `elem` (map fst ds)
   then dependency unixf defs kp op ihi ds srcPath prelPath fs
   else readFirst unixf op srcPath prelPath f >>= \res ->
         case res of 
           Right (t,p,file,source) ->
               readFile source >>= \content->
               let i = filter (`notElem` ihi) (getImports defs file) -- (haskellImport kp file)
                   hash ('\n':'#':_) = True
                   hash ('\n':'%':_) = True
                   hash (_:xs)  = hash xs
                   hash  []     = False
                   cpp = hash ('\n':content)
                   ds' = (f,((t,p,source,cpp),i)):ds
               in
{- Originally, this line was #ifdef sun, but apparently FreeBSD doesn't
   like too many open files either. -}
                  cpp `seq`
                  dependency unixf defs kp op ihi ds' srcPath prelPath (i ++ fs)
           Left _ -> dependency unixf defs kp op ihi ds srcPath prelPath fs

--mtime = itos 0 . head . drop (10::Int) . words
-- where
--   itos a (x:xs) | isDigit x = itos (a*10+ord x - ord '0') xs
--   itos a _ = a

#ifdef __HBC__
readTime f = catch (getFileStat f >>= \sf -> return (At (st_mtime sf)))
                   (\_ -> return Never)
#endif
#if defined(__NHC__) || defined (__GLASGOW_HASKELL__)
readTime f = --hPutStr stderr ("readTime "++f++"\n") >>
             doesFileExist f >>= \so->
             if so then getModificationTime f >>= \mt -> return (At mt)
             else return Never
#endif


readFirst unixf op normalPath prelude filename =
  --hPutStr stderr ("readFirst " ++ show normalPath ++ "\n   " ++ show prelude ++ "\n") >>
  rN normalPath
 where
  ff = fixFileName filename

#ifdef __HBC__
  rN [] = rP prelude 
  rN (p:ps) =
    let source = fixFile unixf p ff "gc"
    in --hPutStr stderr ("Trying (N)" ++ source ++"\n") >>
       catch
         (readFile source >>= \file-> readData p source file)
         (\_-> let source = fixFile unixf p ff "hs"
               in --hPutStr stderr ("Trying (N)" ++ source ++"\n") >>
                  catch
                    (readFile source >>= \file -> readData p source file)
                    (\_ -> let source = fixFile unixf p ff "lhs"
	                   in --hPutStr stderr ("Trying (N)" ++ source ++"\n") >>
                              catch (readFile source >>= \file ->
                                     readData p source (unlit filename file))
                                    (\_ -> rN ps)))

  rP [] = ioError (userError ("Can't find module "++filename++"\nCheck -P and -I flags.\n"))
  rP (p:ps) =
     let source = fixFile unixf p ff "hi"
     in --hPutStr stderr ("Trying (P)" ++ source ++"\n") >>
        catch (readFile source >>= \file ->  return (Left file))
              (\_ -> rP ps)
#endif
#if defined(__NHC__) || defined(__GLASGOW_HASKELL__)
  rN [] = rP prelude 
  rN (p:ps) =
    let try  []        = rN ps
        try ((s,f):xs) = --hPutStr stderr ("Trying (N)" ++ s ++"\n") >>
                         doesFileExist s >>= \so->
                         if so then
                             --hPutStr stderr ("Got (N)" ++ s ++"\n") >>
                             readFile s >>= \file-> readData p s (f file)
                         else try xs
    in try [ (fixFile unixf p ff "gc",  id),
             (fixFile unixf p ff "hs",  id),
             (fixFile unixf p ff "lhs", (unlit filename))]

  rP [] = ioError (userError ("Can't find module "++filename++"\nCheck -P and -I flags.\n"))
  rP (p:ps) =
     let source = fixFile unixf p ff "hi"
     in --hPutStr stderr ("Trying (P)" ++ source ++"\n") >>
        doesFileExist source >>= \so->
        if so then
            --hPutStr stderr ("Got (P)" ++ source ++"\n") >>
            {-readFile source >>= \file ->-} return (Left {-file-}[])
        else rP ps

#endif

  readData path source file =
     readTime source >>= \ths ->
     readTime (fixFile unixf (path{-++op-}) ff "hi") >>= \thi ->
     readTime (fixFile unixf (path++op) ff "o") >>= \tobj ->
     return (Right  ((ths,thi,tobj),path,file,source))

