module IO where

import PreludeBuiltin(Handle)

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
isAlreadyExistsError ioerror = error "Not defined: isAlreadyExistsError"

isDoesNotExistError   :: IOError -> Bool
isDoesNotExistError ioerror = error "Not defined: isDoesNotExistError"

isAlreadyInUseError   :: IOError -> Bool
isAlreadyInUseError ioerror  = error "Not defined: isAlreadyInUseError"

isFullError           :: IOError -> Bool
isFullError ioerror    = error "Not defined: isFullError"

isIllegalOperation    :: IOError -> Bool
isIllegalOperation ioerror = error "Not defined: isIllegalOperation"

isPermissionError     :: IOError -> Bool
isPermissionError ioerror = error "Not defined: isPermissionError"


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

