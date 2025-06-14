module Parse.Int (decodeInt, encodeInt) where

decodeInt :: String -> Maybe Int
decodeInt = decodeInt' (Just 0) where
    decodeInt' acc [] = acc
    decodeInt' acc (h:t) = decodeInt' ((+).(94*) <$> acc <*> translateChar h) t


translateChar :: Char -> Maybe Int
translateChar c
    | n >=33 && n < 127 = Just $ n - 33
    | otherwise = Nothing
    where n = fromEnum c

encodeChar :: Int -> Char
encodeChar n = toEnum (n + 33)

encodeInt :: Int -> String
encodeInt = encodeInt' "" where
    encodeInt' "" 0  = [encodeChar 0]
    encodeInt' acc 0 = acc
    encodeInt' acc n = let (q, r) = divMod n 94 in (encodeInt' $ encodeChar r:acc) q