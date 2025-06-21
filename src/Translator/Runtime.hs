module Translator.Runtime 
    ( reallyEncodeString
    , reallyDecodeInt
    , encodeInt
    , reallyDecodeString
    , Func(..)
    ) where

import Parse.String (encodeString, decodeString)
import Parse.Int (encodeInt, decodeInt)
import Parse (toString)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

reallyEncodeString :: String -> String
reallyEncodeString s = maybe "Encoding Error" toString (encodeString $ T.pack s)

reallyDecodeString :: String -> String
reallyDecodeString s = maybe "Decoding Error" T.unpack (decodeString s)

reallyDecodeInt :: String -> Int
reallyDecodeInt s = fromMaybe (-1337) $ decodeInt s

newtype Func a b = Func { unFunc :: a -> b }