-----------------------------------------------------------------------------
-- |
-- Module      :  PackageConfig
-- Copyright   :  Malcolm Wallace
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Work out the import directories for a bunch of packages.
--
--  * For ghc, we need to consult ghc-pkg for package import directories.
--
--  * nhc98 <= 1.16 stores package imports under its default incdir.
--          >= 1.17 stores package imports under $incdir/packages.
-----------------------------------------------------------------------------

module PackageConfig
  ( packageDirs
  ) where

import Config
import Compiler
import Platform (unsafePerformIO,escape)
import RunAndReadStdout (runAndReadStdout,basename,dirname)
import Directory (doesDirectoryExist)
import IO (hPutStrLn, stderr)
import List (partition,intersperse,isPrefixOf)
import Char (isDigit)
import Monad (when,foldM)

-- | Work out the import directories for a bunch of packages.
packageDirs :: CompilerConfig  -- ^ Which compiler, where it's located, etc.
            -> [String]        -- ^ The packages
            -> [FilePath]

-- For ghc, we need to consult ghc-pkg for package import directories.
packageDirs config@(CompilerConfig{ compilerStyle=Ghc
                                  , compilerPath=ghc }) packages =
    let ghcsym = (read . take 3 . filter isDigit . (++"00") . compilerVersion)
                 config
    in
    if ghcsym < (500::Int) then
      []
    else unsafePerformIO $ do
      pkgcfg <- runAndReadStdout (ghc++" -v 2>&1 | head -2 | tail -1 |"
                                  ++" cut -c28- | head -1")
      let libdir  = dirname (escape pkgcfg)
          incdir1 = libdir++"/imports"
      ok <- doesDirectoryExist incdir1
      if ok
        then do
          let ghcpkg = matching ghc (ghcPkg ghc (compilerVersion config))
       -- pkgs <- runAndReadStdout (ghcpkg++" --list-packages")
          pkgs <- runAndReadStdout (ghcpkg++" -l")
          let (ok,bad) = partition (`elem` deComma pkgs) packages
          when (not (null bad))
               (hPutStrLn stderr ("\nWarning: package(s) "
                                 ++concat (intersperse ", " bad)
                                 ++" not available (according to ghc-pkg)"))
          idirs <- mapM (\p-> runAndReadStdout
                                  (ghcpkg++" --show-package="++p
                                   ++" --field=import_dirs"))
                        ok
          return (pkgDirs libdir idirs)
        else do ioError (userError ("Can't find ghc packages at "++incdir1))
 where
    pkgDirs libdir dirs =
        map (\dir-> if "$libdir" `isPrefixOf` dir
                    then libdir++drop 7 dir
                    else dir)
            (concatMap words dirs)
    deComma pkgs = map (\p-> if last p==',' then init p else p) (words pkgs)
    matching path cmd =
        if '/' `elem` path then dirname path++"/"++cmd else cmd
    ghcPkg ghc ver =
        if '-' `elem` basename ghc then "ghc-pkg-"++ver else "ghc-pkg"

-- nhc98 <= 1.16 stores package imports under its default incdir.
--       >= 1.17 stores package imports under $incdir/packages.
packageDirs config@(CompilerConfig{ compilerStyle=Nhc98
                                  , includePaths=[incdir] }) packages =
  let (pkgdir,base) | compilerVersion config <= "v1.16" = (incdir, [])
                    | otherwise     = (incdir++"/packages", [pkgdir++"/base"])
  in
  unsafePerformIO $ do
    ok <- doesDirectoryExist pkgdir
    if ok
      then do
        (good,bad) <- foldM (\(g,b) d->
                             do let dir = pkgdir++"/"++d
                                ok <- doesDirectoryExist dir
                                return (if ok then (dir:g, b) else (g, d:b)))
                            (base,[]) packages
        when (not (null bad))
             (hPutStrLn stderr ("\nWarning: package(s) "
                               ++concat (intersperse ", " bad)
                               ++" not available in "++pkgdir))
        return good
      else ioError (userError ("Can't find nhc98 packages at "++pkgdir))

-- No other compiler supports packages.
packageDirs config packages = []
