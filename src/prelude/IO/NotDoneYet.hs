module IO where

import PreludeBuiltin(Handle)

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

hReady                :: Handle -> IO Bool 
hReady h               = error "Not defined: hReady"

hLookAhead            :: Handle -> IO Char
hLookAhead h           = error "Not defined: hLookAhead"

isAlreadyExistsError  :: IOError -> Bool
isAlreadyExistsError ioerror = error "Not defined: isAlreadyExistsError"

isAlreadyInUseError   :: IOError -> Bool
isAlreadyInUseError ioerror  = error "Not defined: isAlreadyInUseError"

isFullError           :: IOError -> Bool
isFullError ioerror = error "Not defined: isFullError"

isIllegalOperation    :: IOError -> Bool
isIllegalOperation ioerror = error "Not defined: isIllegalOperation"

isPermissionError     :: IOError -> Bool
isPermissionError ioerror = error "Not defined: isPermissionError"
