module Translated.Efficiency.Efficiency4 where

import Translator (reallyEncodeString, reallyDecodeInt, reallyDecodeString, encodeInt)

result = f2 40

f1 :: (Int -> Int) -> Int -> Int
f1 v3 v4 = if v4 < 2 then 1 else v3 (v4 - 1) + v3 (v4 - 2)

f2 :: Int -> Int
f2 = f3 (Func f3)

f3 :: Func -> Int -> Int
f3 f@(Func v2) = f1 (v2 f)

newtype Func = Func (Func -> Int -> Int)