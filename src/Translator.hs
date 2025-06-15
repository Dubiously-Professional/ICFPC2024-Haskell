module Translator (translate, reallyEncodeString, reallyDecodeInt, encodeInt, reallyDecodeString) where

import Parse (tokenize, Parser, runParser, failParse, ParseError (InvalidBody, SyntaxError), parseToken, toString)
import Parse.String (decodeString, encodeString)
import Parse.Int (decodeInt, encodeInt)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

maybeParse :: Maybe a -> Parser a
maybeParse = maybe (failParse InvalidBody) return

translateUnary :: String -> Parser String
translateUnary op = do
    arg <- translateExpression
    return $ "(" ++ op ++ " " ++ arg ++ ")"

translateInfix :: String -> Parser String
translateInfix op = do
    arg1 <- translateExpression
    arg2 <- translateExpression
    return $ "(" ++ arg1 ++ " " ++ op ++ " " ++ arg2 ++ ")"

translateExpression :: Parser String
translateExpression = do
    token <- parseToken
    translateToken token

translateIfThen :: Parser String
translateIfThen = do
    cond <- translateExpression
    t <- translateExpression
    f <- translateExpression
    return $ "(if " ++ cond ++ " then " ++ t ++ " else " ++ f ++ ")"

translateLambda :: String -> Parser String
translateLambda arg = do
    body <- translateExpression
    return $ "(\\v" ++ arg ++ " -> " ++ body ++ ")"

reallyEncodeString :: String -> String
reallyEncodeString s = maybe "Encoding Error" toString (encodeString $ T.pack s)

reallyDecodeString :: String -> String
reallyDecodeString s = maybe "Decoding Error" T.unpack (decodeString s)

reallyDecodeInt :: String -> Int
reallyDecodeInt s = fromMaybe (-1337) $ decodeInt s

translateToken :: String -> Parser String
translateToken "T" = return "True"
translateToken "F" = return "False"
translateToken ('I':body) = maybeParse $ show <$> decodeInt body
translateToken ('S':body) = maybeParse $ Just "\"" <++> (T.unpack <$> decodeString body) <++> Just "\""
translateToken "U-" = translateUnary "negate"
translateToken "U!" = translateUnary "not"
translateToken "U#" = translateUnary "reallyDecodeInt $ reallyEncodeString"
translateToken "U$" = translateUnary "reallyDecodeString $ encodeInt"
translateToken "B/" = translateInfix "`quot`"
translateToken "B%" = translateInfix "`rem`"
translateToken "B=" = translateInfix "=="
translateToken "B|" = translateInfix "||"
translateToken "B&" = translateInfix "&&"
translateToken "B." = translateInfix "++"
translateToken "BT" = translateInfix "`take`"
translateToken "BD" = translateInfix "`drop`"
translateToken "B!" = translateInfix "$!"
translateToken "B~" = translateInfix "$"
translateToken ('B':body) = translateInfix body
translateToken "?" = translateIfThen
translateToken ('L':body) = maybeParse (show <$> decodeInt body) >>= translateLambda
translateToken ('v':body) = maybeParse $ Just "v" <++> (show <$> decodeInt body)
translateToken x = failParse $ SyntaxError x

makeHaskell :: LBS.ByteString -> Maybe String
makeHaskell icfp = let initialState = tokenize icfp
  in case runParser translateExpression initialState of
    Right (haskell, _) -> Just haskell
    Left _            -> Nothing

translate :: LBS.ByteString -> Maybe String
translate response = Just headers <++> makeHaskell response

headers :: String
headers = "import Translator (reallyEncodeString, reallyDecodeInt, reallyDecodeString, encodeInt)\n\n"