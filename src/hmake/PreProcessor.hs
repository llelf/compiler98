-- This module describes various preprocessors for the Haskell language.
-- If you write a new preprocessor, please add details here.
module PreProcessor where

import Argv
import Compiler
import Config
import Unlit (unlit,plain)

data PreProcessor = PreProcessor
	{ ppExecutableName   :: String
        , ppDefaultOptions   :: DecodedArgs -> [String]
	, ppOutputFileOption :: Maybe String
	, ppSuitable         :: HC -> Bool
	}


knownSuffixes :: [ ( String			-- input file suffix 
                   , (String->String->String)  	-- unliterate the file?
                   , PreProcessor ) ]		-- PP info
knownSuffixes =
  [ ("gc",     plain, ppGreenCard)
  , ("chs",    plain, ppC2hs)
  , ("hsc",    plain, ppHsc2hs)
  , ("y",      plain, ppHappy)
  , ("ly",     unlit, ppHappy)
  , ("hs.cpp", plain, ppCpp)
  , ("gc",     plain, ppNone)	-- note, for nhc98 only
  , ("hs",     plain, ppNone)
  , ("lhs",    unlit, ppNone)
  ] 

ppCpp, ppGreenCard, ppHsc2hs, ppC2hs, ppHappy, ppNone :: PreProcessor
ppCpp = PreProcessor
	{ ppExecutableName = "gcc -E"
	, ppDefaultOptions = \d-> "-x c" : map ("-D"++) (defs d++zdefs d)
	, ppOutputFileOption = Just "-o "
	, ppSuitable = \hc-> True
	}
ppGreenCard = PreProcessor
	{ ppExecutableName = "greencard"
	, ppDefaultOptions = \d-> ["-t", show (compilerStyle (compiler d))]
	, ppOutputFileOption = Just "-o "
	, ppSuitable = \hc-> hc == Ghc
	}
ppHsc2hs = PreProcessor
	{ ppExecutableName = "hsc2hs"
	, ppDefaultOptions = \_-> []
	, ppOutputFileOption = Nothing
	, ppSuitable = \hc-> hc `elem` [Ghc,Nhc98]
	}
ppC2hs = PreProcessor
	{ ppExecutableName = "c2hs"
	, ppDefaultOptions = \_-> []
	, ppOutputFileOption = Nothing
	, ppSuitable = \hc-> hc `elem` [Ghc,Nhc98]
	}
ppHappy = PreProcessor
	{ ppExecutableName = "happy"
	, ppDefaultOptions = \_-> []
	, ppOutputFileOption = Nothing
	, ppSuitable = \hc-> True
	}
ppNone = PreProcessor
	{ ppExecutableName = ""
	, ppDefaultOptions = \_-> []
	, ppOutputFileOption = Nothing
	, ppSuitable = \hc-> True
	}

