module Main where

import IO
import Binary
import Directory
import System

data ZooTree = ZT (BinPtr ZooTree) (BinPtr ZooTree) String
               deriving Binary

main =
  doesFileExist "zoo" >>= \exists->
  openBin (File "zoo" RW) >>= \zoo ->
  (if exists then
      get zoo
  else
      put zoo (0,0) >>
      put zoo (ZT 0 0 "dog") >>= \root->
      tellBin zoo >>= \eof->
      putAt zoo 0 (root,eof) >>
      return (root,eof)
  ) >>= \(root,eof) ->
  untilCatch isEOFError 
    ( \(p, z, end) ->
     (if p==0 then
        qanda "Have you thought of an animal"
          (return ()) 
          (putAt zoo (unsafeShiftBinPtr sizeofBinPtr 0) end >> 
           closeBin zoo >>
           exitWith ExitSuccess)
      else return ()
      ) >>
      getAt zoo z >>= \(ZT y n s) ->
      if (y == 0) then 
        qanda ("Is it a "++s++"?")
          ( getAt zoo 0 >>= \newroot->
            return (0, newroot, end) )
          ( putStrLn "What is it then?" >>
            getLine >>= \t ->
            putStrLn ("What question has answer yes for a "++s++
                      " but no for a "++t) >>
            getLine >>= \q ->
            putAt zoo end (ZT 0 0 t) >>
            put zoo (ZT z end q) >>= \qpos->
            tellBin zoo >>= \newend-> 
            putAt zoo p qpos >>
            getAt zoo 0 >>= \newroot->
            return (0, newroot, newend) )
      else
        qanda s
          (return (unsafeShiftBinPtr 0            z, y, end))
          (return (unsafeShiftBinPtr sizeofBinPtr z, n, end))
    )
    (0 :: BinPtr (BinPtr ZooTree), root, eof)
    >>= \(_,_,end)->
    putAt zoo (unsafeShiftBinPtr sizeofBinPtr 0) end >> 
    closeBin zoo >>
    exitWith ExitSuccess

qanda :: String -> (IO a) -> (IO a) -> (IO a)
qanda q y n =
  putStrLn q >>
  getLine >>= \(a:_) ->
  case a of
    'y' -> y
    'n' -> n 
    _   -> putStrLn "Start answer y or n." >> qanda q y n


------
untilCatch  :: (IOError->Bool) -> (a->IO a) -> a -> IO a
untilCatch_ :: (IOError->Bool) ->     IO () ->      IO ()

untilCatch p f a =
  catch (f a >>= \x-> return (True,x))
        (\e-> if p e then return (False,a) else fail e)
  >>= \(ok,a')->
  if ok then untilCatch p f a' else return a'

untilCatch_ p f =
  catch (f >> return True)
        (\e-> if p e then return False else fail e)
  >>= \ok->
  if ok then untilCatch_ p f else return ()

------
