module Directory
  ( Permissions(..)
  , createDirectory, removeDirectory, removeFile
  , renameDirectory, renameFile, getDirectoryContents
  , getCurrentDirectory, setCurrentDirectory
  , doesFileExist, doesDirectoryExist, getModificationTime
  ) where

import Warning
import DPermissions
import DoesFileExist
import DoesDirectoryExist
import GetModificationTime

createDirectory         :: FilePath -> IO ()
createDirectory fp       = warning "Not defined: createDirectory"
                               (return ())

removeDirectory         :: FilePath -> IO ()
removeDirectory fp       = warning "Not defined: removeDirectory"
                               (return ())

removeFile              :: FilePath -> IO ()
removeFile fp            = warning "Not defined: removeFile"
                               (return ())

renameDirectory         :: FilePath -> FilePath -> IO ()
renameDirectory fp1 fp2  = warning "Not defined: renameDirectory"
                               (return ())

renameFile              :: FilePath -> FilePath -> IO ()
renameFile fp1 fp2       = warning "Not defined: renameFile"
                               (return ())

getDirectoryContents    :: FilePath -> IO [FilePath]
getDirectoryContents fp  = warning "Not defined: getDirectoryContents"
                               (return [])

getCurrentDirectory     :: IO FilePath
getCurrentDirectory      = warning "Not defined: getCurrentDirectory"
                               (return "")

setCurrentDirectory     :: FilePath -> IO ()
setCurrentDirectory fp   = warning "Not defined: setCurrentDirectory"
                               (return ())

getPermissions          :: FilePath -> IO Permissions
getPermissions fp        = warning "Not defined: getPermissions"
                               (error "sorry, can't continue.")

setPermissions          :: FilePath -> Permissions -> IO ()
setPermissions fp p      = warning "Not defined: setPermissions"
                               (return ())

