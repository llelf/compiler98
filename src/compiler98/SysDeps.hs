module SysDeps (
   module PS
) where

#if __GLASGOW_HASKELL__ >= 502
import Data.PackedString as PS
#else
import PackedString as PS
#endif
