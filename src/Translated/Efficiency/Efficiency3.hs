module Translated.Efficiency.Efficiency3 where

import Translator (reallyEncodeString, reallyDecodeInt, reallyDecodeString, encodeInt)

f1 :: (Int -> Int) -> Int -> Int
f1 v3 v4 = if v4 == 0 then 1 else 1 + v3 (v4 - 1)

f2 :: Int -> Int
f2 = f3 (Func f3)

f3 :: Func -> Int -> Int
f3 f@(Func v2) = f1 (v2 f)

newtype Func = Func (Func -> Int -> Int)

result = 2134 + f2 3