module Prelude 
    (module _ReadCon
    ,module _ReadCon0
    ,module _ReadConArg
    ,module _ReadConInfix
    ,module _ReadField

    ,module CRead
    ,module CShow
    ,module Lex
    ,module Read
    ,module ReadParen
    ,Read(..)
    ,module Reads
    ,module ShowType
    ,module ShowChar
    ,module ShowParen
    ,module ShowString
    ,Show(..)
    ,module Shows
    ,module TReadS
    ,module TShowS
    ) where

-- Shouldn't be visible, but they are:-(
import _ReadCon
import _ReadCon0
import _ReadConArg
import _ReadConInfix
import _ReadField


import CRead
import CShow
import Lex
import Read
import ReadParen
import Read_2
import Read_3
import Read_4
import Read_Bool
import Read_Char
import Read_Int
import Read_Double
import Read_Float
import Read_Integer
import Read_Either
import Read_List
import Read_Maybe
import Read_Ordering
import Read_Unit
import Reads
import ShowType
import ShowChar
import ShowParen
import ShowString
import Show_2
import Show_3
import Show_4
import Show_Bool
import Show_Char
import Show_Double
import Show_Either
import Show_Float
--import Show_Fun
import Show_IO
import Show_Int
import Show_Integer
import Show_List
import Show_Maybe
import Show_Ordering
import Show_Unit
import Shows
import TReadS
import TShowS

