{-# LANGUAGE OverloadedStrings #-}
module Parse.String (decodeString) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Word (Word8)

-- Translation table for codes 33-126 (94 characters)
translationTable :: String
translationTable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

decodeString :: LBS.ByteString -> Maybe T.Text
decodeString lbs = 
    let bytes = LBS.unpack lbs
    in fmap T.pack (mapM translateByte bytes)

translateByte :: Word8 -> Maybe Char
translateByte byte
    | byte >= 33 && byte <= 126 = 
        let index = fromIntegral byte - 33
        in Just (translationTable !! index)
    | otherwise = Nothing
