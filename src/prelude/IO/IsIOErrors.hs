module IO where

import DIOError
import DErrNo

isAlreadyExistsError  :: IOError -> Bool
isAlreadyExistsError (IOErrorC _ _ errno) = errno `elem` alreadyexists

isDoesNotExistError   :: IOError -> Bool
isDoesNotExistError (IOErrorC _ _ errno)  = errno `elem` doesnotexist

isAlreadyInUseError   :: IOError -> Bool
isAlreadyInUseError (IOErrorC _ _ errno)  = errno `elem` alreadyinuse

isFullError           :: IOError -> Bool
isFullError (IOErrorC _ _ errno)          = errno `elem` full

isIllegalOperation    :: IOError -> Bool
isIllegalOperation (IOErrorC _ _ errno)   = errno `elem` illegalop

isPermissionError     :: IOError -> Bool
isPermissionError (IOErrorC _ _ errno)    = errno `elem` nopermission

