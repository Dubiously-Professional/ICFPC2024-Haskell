module Translated.Efficiency.Efficiency4 where

import Translator (reallyEncodeString, reallyDecodeInt, reallyDecodeString, encodeInt)

result = f1 40

f1 :: Int -> Int
f1 v4 = if v4 < 2 then 1 else f1 (v4 - 1) + f1 (v4 - 2)

newtype Func = Func (Func -> Int -> Int)