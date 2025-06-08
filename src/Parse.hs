module Parse (Expression, parse) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Parse.String (decodeString)

data Expression = StringExpression LBS.ByteString

decode :: Expression -> Maybe T.Text
decode (StringExpression raw) = decodeString raw
decode _ = Nothing

tokenize :: LBS.ByteString -> Maybe Expression
tokenize raw = do
  (pfx, sfx) <- LBS.uncons raw
  case pfx of
    83 -> return $ StringExpression sfx -- ASCII S
    _  -> Nothing

parse :: LBS.ByteString -> Maybe T.Text
parse raw = tokenize raw >>= decode
