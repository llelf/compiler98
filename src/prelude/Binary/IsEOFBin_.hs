module Binary
  ( isEOFBin
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_isEOFBin primitive 1 :: ForeignObj -> Bool

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BH bh) =
  IO (\_ ->
	let gr_result = gr_isEOFBin bh
	in seq gr_result (Right gr_result))



-- This could return the wrong result.  Check whether the underlying
-- C routines reset the eof flag correctly following a forceCacheTo
-- (or other) movement.  Checked OK 980325.