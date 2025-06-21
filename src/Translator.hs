module Translator (translate) where

import Parse (tokenize, Parser, runParser, failParse, ParseError (InvalidBody, SyntaxError), parseToken)
import Parse.String (decodeString)
import Parse.Int (decodeInt)
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
    return $ "(ifThenElse " ++ cond ++ " " ++ t ++ " " ++ f ++ ")"

translateLambda :: String -> Parser String
translateLambda arg = do
    body <- translateExpression
    return $ "(mkFunc $ \\v" ++ arg ++ " -> " ++ body ++ ")"

translateCall :: String -> Parser String
translateCall op = do
    func <- translateExpression
    arg <- translateExpression
    return $ "applyFunc (" ++ func ++ ") (" ++ arg ++ ")"

translateToken :: String -> Parser String
translateToken "T" = return "(mkBool True)"
translateToken "F" = return "(mkBool False)"
translateToken ('I':body) = maybeParse $ (\x -> "(mkInt " ++ show x ++ ")") <$> decodeInt body
translateToken ('S':body) = maybeParse $ Just "(mkString \"" <++> (T.unpack <$> decodeString body) <++> Just "\")"
translateToken "U-" = translateUnary "negateVal"
translateToken "U!" = translateUnary "notVal"
translateToken "U#" = translateUnary "strToInt"
translateToken "U$" = translateUnary "intToStr"
translateToken "B/" = translateInfix "`quotVal`"
translateToken "B%" = translateInfix "`remVal`"
translateToken "B=" = translateInfix "`eqVal`"
translateToken "B|" = translateInfix "`orVal`"
translateToken "B&" = translateInfix "`andVal`"
translateToken "B." = translateInfix "`concatVal`"
translateToken "BT" = translateInfix "`takeVal`"
translateToken "BD" = translateInfix "`dropVal`"
translateToken "B$" = translateCall "$"
translateToken "B!" = translateCall "$!"
translateToken "B~" = translateCall "$"
translateToken "B+" = translateInfix "`addVal`"
translateToken "B-" = translateInfix "`subVal`"
translateToken "B*" = translateInfix "`mulVal`"
translateToken "B<" = translateInfix "`ltVal`"
translateToken "B>" = translateInfix "`gtVal`"
translateToken ('B':body) = error $ "Unknown binary operator: B" ++ body
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
headers = "import Translator.Runtime\n\nresult = extractResult $ "
