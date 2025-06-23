module Utils (toString) where

import qualified Data.ByteString.Lazy as LBS

toString :: LBS.ByteString -> String
toString raw = map (toEnum . fromIntegral) $ LBS.unpack raw
