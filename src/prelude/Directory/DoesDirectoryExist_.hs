module Directory where

import GreenCard



gr_doesDirectoryExist primitive 1 :: PackedString -> Bool

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist tmp1 =
  IO (\_ ->
	let fp = toCString tmp1
	    gr_result = gr_doesDirectoryExist fp
	in seq gr_result (Right gr_result))


