module Binary
  ( copyBin
  ) where

import GreenCard
import BinHandle   ({-type-}BinHandle(..))
import BinLocation ({-type-}BinLocation)
import OpenBin     (openBin)
import SizeBin     (sizeBin)

    -- %-#include <stdio.h>

copyBin :: BinHandle -> BinLocation -> IO BinHandle
copyBin sbh loc =
  openBin loc >>= \dbh->
  sizeBin sbh >>= \nbytes->
  copyBinAux sbh dbh nbytes >>
  return dbh

gr_copyBinAux primitive 3 :: ForeignObj -> ForeignObj -> Int -> ()

copyBinAux :: BinHandle -> BinHandle -> Int -> IO ()
copyBinAux (BH sbh) (BH dbh) bytes =
  IO (\_ ->
	let gr_result = gr_copyBinAux sbh dbh bytes
	in seq gr_result (Right gr_result))


