module IO where

import PreludeBuiltin(Handle)
import DIOError
import DErrNo

hWaitForInput         :: Handle -> Int -> IO Bool
hWaitForInput h n      = error "Not defined: hWaitForInput"

hReady                :: Handle -> IO Bool
hReady h               = hWaitForInput h 0

hLookAhead            :: Handle -> IO Char
hLookAhead h           = error "Not defined: hLookAhead"

hIsOpen               :: Handle -> IO Bool
hIsOpen h              = error "Not defined: hIsOpen"

hIsClosed             :: Handle -> IO Bool
hIsClosed h            = error "Not defined: hIsClosed"

hIsReadable           :: Handle -> IO Bool
hIsReadable h          = error "Not defined: hIsReadable"

hIsWritable           :: Handle -> IO Bool
hIsWritable h          = error "Not defined: hIsWritable"

hIsSeekable           :: Handle -> IO Bool
hIsSeekable h          = error "Not defined: hIsSeekable"


isAlreadyExistsError  :: IOError -> Bool
isAlreadyExistsError (IOErrorC EEXIST) = True	-- file exists
isAlreadyExistsError (IOErrorC EISDIR) = True	-- is a directory
isAlreadyExistsError _ = False

isDoesNotExistError   :: IOError -> Bool
isDoesNotExistError (IOErrorC ENOENT) = True	-- no such file or directory
isDoesNotExistError (IOErrorC ENXIO)  = True	-- no such device or address
isDoesNotExistError _ = False

isAlreadyInUseError   :: IOError -> Bool
isAlreadyInUseError (IOErrorC EBUSY)    = True	-- Device or resource busy
isAlreadyInUseError (IOErrorC ETXTBSY)  = True	-- Text file busy
isAlreadyInUseError _  = False

isFullError           :: IOError -> Bool
isFullError (IOErrorC ENOSPC)    = True		-- device full
isFullError (IOErrorC EDQUOT)    = True		-- quota exceeded
isFullError _    = False

isIllegalOperation    :: IOError -> Bool
isIllegalOperation (IOErrorC EPERM) = True
isIllegalOperation _ = False

isPermissionError     :: IOError -> Bool
isPermissionError (IOErrorC EACCES) = True
isPermissionError _ = False


-- The following implementations are direct from the Library Report.

try                   :: IO a -> IO (Either IOError a)
try f                  = catch (do r <- f
                                   return (Right r))
                               (return . Left)

bracket               :: IO a -> (a->IO b) -> (a->IO c) -> IO c
bracket before after m     = do
    x <- before
    rs <- try (m x)
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

bracket_              :: IO a -> (a->IO b) -> IO c -> IO c
bracket_ before after m     = do
    x <- before
    rs <- try m
    after x
    case rs of
      Right r -> return r
      Left e  -> ioError e

