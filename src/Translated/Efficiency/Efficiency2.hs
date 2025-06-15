module Translated.Efficiency.Efficiency2 where

import Translator (reallyEncodeString, reallyDecodeInt, reallyDecodeString, encodeInt)

f1 :: (Int -> Int) -> Int -> Int
f1 v3 v4 = if v4 == 0 then 1 else 1 + v3 (v4 - 1)

result = 2134 -- + (\v1 -> (\v2 -> v1 (v2 v2)) (\v2 -> v1 (v2 v2))) f1 9345873499 * 0
