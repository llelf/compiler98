module Directory where

data Permissions = Permissions {
    readable, writable, executable, searchable :: Bool
    } deriving (Eq, Ord, Read, Show)

