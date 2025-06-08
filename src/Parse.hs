module Parse (Expression, parse, makeString, decode, encode, unparse) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Parse.String (decodeString, encodeString)

data Expression = StringExpression LBS.ByteString deriving Show

decode :: Expression -> Maybe T.Text
decode (StringExpression raw) = decodeString raw
decode _ = Nothing

encode :: Expression -> LBS.ByteString
encode (StringExpression raw) = LBS.cons 83 $ raw -- ASCII S

tokenize :: LBS.ByteString -> Maybe Expression
tokenize raw = do
  (pfx, sfx) <- LBS.uncons raw
  case pfx of
    83 -> return $ StringExpression sfx -- ASCII S
    _  -> Nothing

parse :: LBS.ByteString -> Maybe T.Text
parse raw = tokenize raw >>= decode

makeString :: T.Text -> Maybe Expression
makeString msg = StringExpression <$> encodeString msg

unparse :: T.Text -> Maybe LBS.ByteString
unparse msg = encode <$> makeString msg
