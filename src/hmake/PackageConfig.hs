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

-- Work out the import directories for a bunch of packages.
packageDirs :: CompilerConfig -> [String] -> [FilePath]

-- For ghc, we need to consult ghc-pkg for package import directories.
packageDirs config@(CompilerConfig{ compilerStyle=Ghc
                                  , compilerPath=ghc }) packages =
    let ghcsym = (read . take 3 . filter isDigit . compilerVersion) config in
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
          pkgs <- runAndReadStdout (ghcpkg++" --list-packages")
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

-- nhc98 always stores package imports under its default incdir.
packageDirs config@(CompilerConfig{ compilerStyle=Nhc98
                                  , includePaths=[incdir] }) packages =
  unsafePerformIO $ do
    ok <- doesDirectoryExist incdir
    if ok
      then do
        (good,bad) <- foldM (\(g,b) d->
                             do let dir = incdir++"/"++d
                                ok <- doesDirectoryExist dir
                                return (if ok then (dir:g, b) else (g, d:b)))
                            ([],[]) packages
        when (not (null bad))
             (hPutStrLn stderr ("\nWarning: package(s) "
                               ++concat (intersperse ", " bad)
                               ++" not available in "++incdir))
        return good
      else ioError (userError ("Can't find nhc98 packages at "++incdir))

-- No other compiler supports packages.
packageDirs config packages = []
