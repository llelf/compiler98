module IO where

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                     deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)
