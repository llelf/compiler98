module TPrelude
  (g_filter,g_foldr,gmap,(!++),gfilter,gconcat,ghead,glast,gtail,ginit,gnull
    ,glength,(!!!),gfoldl,gfoldl1,gscanl,gscanl1,gfoldr,gfoldr1,gscanr,gscanr1
    ,giterate,grepeat,greplicate,gcycle,gtake,gdrop,gsplitAt,gtakeWhile
    ,gdropWhile,gspan,gbreak,glines,gwords,gunlines,gunwords,greverse,gand,gor
    ,gany,gall,gelem,gnotElem,glookup,gsum,gproduct,gmaximum,gminimum,gconcatMap
    ,gzip,gzip3,gzipWith,gzipWith3,gunzip,gunzip3,ReadS(),ShowS()
    ,Read(greadsPrec,greadList),Show(gshowsPrec,gshowList),greads,gshows,gshow
    ,gread,glex,gshowChar,gshowString,greadParen,gshowParen,FilePath(),IOError()
    ,gioError,guserError,gcatch,gputChar,gputStr,gputStrLn,gprint,ggetChar
    ,ggetLine,ggetContents,ginteract,greadFile,gwriteFile,gappendFile,greadIO
    ,greadLn,Bool(False,True),aFalse,aTrue,Maybe(Nothing,Just),aNothing,aJust
    ,Either(Left,Right),aLeft,aRight,Ordering(LT,EQ,GT),aLT,aEQ,aGT,Char()
    ,String(),Int(),Integer(),Float(),Double(),Rational(),IO(),T.List(T.Cons
      ,T.List),T.aCons,T.aList,T.Tuple2(),T.Tuple3(),T.Tuple4(),T.Tuple5()
    ,T.Tuple6(),T.Tuple7(),T.Tuple8(),T.Tuple9(),T.Tuple10(),T.Tuple11()
    ,T.Tuple12(),T.Tuple13(),T.Tuple14(),T.Tuple15(),T.Tuple0(),T.Fun(),Eq((!==)
      ,(!/=)),Ord(gcompare,(!<),(!<=),(!>=),(!>),gmax,gmin),Enum(gsucc,gpred
      ,gtoEnum,gfromEnum,genumFrom,genumFromThen,genumFromTo,genumFromThenTo)
    ,Bounded(gminBound,gmaxBound),Num((!+),(!-),(!*),gnegate,gabs,gsignum
      ,gfromInteger),Real(gtoRational),Integral(gquot,grem,gdiv,gmod,gquotRem
      ,gdivMod,gtoInteger),Fractional((!/),grecip,gfromRational),Floating(gpi
      ,gexp,glog,gsqrt,(!**),glogBase,gsin,gcos,gtan,gasin,gacos,gatan,gsinh
      ,gcosh,gtanh,gasinh,gacosh,gatanh),RealFrac(gproperFraction,gtruncate
      ,ground,gceiling,gfloor),RealFloat(gfloatRadix,gfloatDigits,gfloatRange
      ,gdecodeFloat,gencodeFloat,gexponent,gsignificand,gscaleFloat,gisNaN
      ,gisInfinite,gisDenormalized,gisIEEE,gisNegativeZero,gatan2),Monad((!>>=)
      ,(!>>),greturn,gfail),Functor(gfmap),gmapM,gmapM_,gsequence,gsequence_
    ,(!=<<),gmaybe,geither,(!&&),(!||),gnot,gotherwise,gsubtract,geven,godd,ggcd
    ,glcm,(!^),(!^^),gfromIntegral,grealToFrac,gfst,gsnd,gcurry,guncurry,gid
    ,gconst,(!.),gflip,(!$),guntil,gasTypeOf,gerror,gundefined,gseq,(!$!)) where

import qualified Prelude 
import qualified Hat as T 
import TPreludeBasic 

tPrelude = T.mkModule "Prelude" "./PreludeTracing.hs" Prelude.True
