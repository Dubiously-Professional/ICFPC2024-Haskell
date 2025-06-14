module Translator (translate) where

import Parse (tokenize)
import Parse.String (decodeString)
import Parse.Int (decodeInt)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

(<++>) :: Maybe [a] -> Maybe [a] -> Maybe [a]
(<++>) = liftA2 (++)


translateToken :: String -> Maybe String
translateToken "T" = Just "True"
translateToken "F" = Just "False"
translateToken ('I':body) = show <$> decodeInt body
translateToken ('S':body) = Just "\"" <++> (T.unpack <$> decodeString body) <++> Just "\""
translateToken "U-" = Just "negate"
translateToken "U!" = Just "not"
translateToken "U#" = Just "read"
translateToken "U$" = Just "show"
translateToken "B=" = Just "(==)"
translateToken "B|" = Just "(||)"
translateToken "B&" = Just "(&&)"
translateToken "B." = Just "(++)"
translateToken "BT" = Just "take"
translateToken "BD" = Just "drop"
translateToken ('B':body) = Just $ "(" ++ body ++ ")"
translateToken "?" = Just "ifthen"
translateToken ('L':body) = Just "\\v" <++> (show <$> decodeInt body) <++> Just " ->"
translateToken ('v':body) = Just "v" <++> (show <$> decodeInt body)
translateToken _ = Nothing

headers :: String
headers = "ifthen cond t f = if cond then t else f\n"

makeHaskell :: LBS.ByteString -> Maybe String
makeHaskell icfp = let
    tokens = tokenize icfp
    tTokens = mapM translateToken tokens
    in fmap unwords tTokens

translate :: LBS.ByteString -> Maybe String
translate response = Just headers <++> makeHaskell response