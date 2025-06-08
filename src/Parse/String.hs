{-# LANGUAGE OverloadedStrings #-}
module Parse.String (decodeString) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Array (Array, array, (!), bounds, inRange)

-- Translation table for codes 33-126 (94 characters)                                                                                                 
translationTable :: String
translationTable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

-- Pre-computed array for O(1) lookups
translationArray :: Array Int Char
translationArray = array (33, 126) (zip [33..126] translationTable)

decodeString :: LBS.ByteString -> Maybe T.Text
decodeString lbs =
    let bytes = LBS.unpack lbs
    in fmap T.pack (mapM translateByte bytes)

translateByte :: Word8 -> Maybe Char
translateByte byte
    | inRange (bounds translationArray) index = Just (translationArray ! index)
    | otherwise = Nothing
  where
    index = fromIntegral byte
