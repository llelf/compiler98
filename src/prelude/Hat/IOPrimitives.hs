module IOPrimitives where

data Handle = ...
data HandlePosn = ...

instance Eq Handle
instance Eq HandlePosn

instance Show Handle
instance Show HandlePosn

stdin, stdout, stderr	:: Handle

hClose		:: Handle -> IO ()
hFileSize	:: Handle -> IO Integer
hFlush		:: Handle -> IO ()
hGetBuffering	:: Handle -> IO BufferMode
hGetChar	:: Handle -> IO Char
hGetContents	:: Handle -> IO String
hGetPosn	:: Handle -> IO HandlePosn
hIsEOF		:: Handle -> IO Bool
hPutChar	:: Handle -> Char -> IO ()
hSeek		:: Handle -> SeekMode -> Integer -> IO ()
hSetBuffering	:: Handle  -> BufferMode -> IO ()
hSetPosn	:: HandlePosn -> IO ()
ioeGetErrorString :: IOError -> String
ioeGetFileName	:: IOError -> Maybe FilePath
ioeGetHandle	:: IOError -> Maybe Handle
isEOFError	:: IOError -> Bool
isAlreadyExistsError	:: IOError -> Bool
isDoesNotExistError	:: IOError -> Bool
isAlreadyInUseError	:: IOError -> Bool
isFullError		:: IOError -> Bool
isIllegalOperation	:: IOError -> Bool
isPermissionError	:: IOError -> Bool
isUserError		:: IOError -> Bool
hWaitForInput	:: Handle -> Int -> IO Bool
hReady		:: Handle -> IO Bool
hLookAhead	:: Handle -> IO Char
hIsOpen		:: Handle -> IO Bool
hIsClosed	:: Handle -> IO Bool
hIsReadable	:: Handle -> IO Bool
hIsWritable	:: Handle -> IO Bool
hIsSeekable	:: Handle -> IO Bool
openFile	:: FilePath -> IOMode -> IO Handle

