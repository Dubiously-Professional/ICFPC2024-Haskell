module Translated.Efficiency.Efficiency2 where

import Translator.Runtime

f1 :: Func (Func Int Int) (Func Int Int)
f1 = Func $ \v3 -> Func (\v4 -> (if v4 == 0 then 1 else 1 + unFunc v3 $ v4 - 1))

--result = 2134 -- + (\v1 -> (\v2 -> v1 (v2 v2)) (\v2 -> v1 (v2 v2))) f1 9345873499 * 0

result = 2134 + unFunc f2 f1 (9345873499 * 0)

--f2 :: (Func (Func Int Int) (Func Int Int)) -> Func (Func)
f2 v1 v2 = unFunc (Func $ \v2 -> unFunc v1 $ unFunc (v2 :: Func (Func a (Int->Int)) (Int->Int)) v2) (Func $ \v2 -> unFunc v1 (unFunc v2 v2)) 