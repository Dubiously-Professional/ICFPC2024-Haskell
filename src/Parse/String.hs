{-# LANGUAGE OverloadedStrings #-}
module Parse.String (decodeString, encodeString) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Array (Array, array, (!), bounds, inRange)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Translation table for codes 33-126 (94 characters)                                                                                                 
translationTable :: String
translationTable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

-- Pre-computed array for O(1) lookups
translationArray :: Array Word8 Char
translationArray = array (33, 126) (zip [33..126] translationTable)

-- Pre-computed map for O(log n) lookups (encoding)
reverseTranslationMap :: Map Char Word8
reverseTranslationMap = Map.fromList (zip translationTable [33..126])

decodeString :: String -> Maybe T.Text
decodeString raw =
  let bytes = map (fromIntegral.fromEnum) raw
    in fmap T.pack (mapM translateByte bytes)

translateByte :: Word8 -> Maybe Char
translateByte byte
    | inRange (bounds translationArray) index = Just (translationArray ! index)
    | otherwise = Nothing
  where
    index = byte

encodeString :: T.Text -> Maybe LBS.ByteString
encodeString text =
    let chars = T.unpack text
    in fmap LBS.pack (mapM translateChar chars)

translateChar :: Char -> Maybe Word8
translateChar char = Map.lookup char reverseTranslationMap
