{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Communicator (sendRaw, sendText) where

import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import Parse (unparse)

bearerToken :: String
bearerToken = "00000000-0000-0000-0000-000000000000"

communicateUrl :: String
communicateUrl = "http://localhost:8000/communicate"

-- | Send a POST request with bearer token authentication
postWithBearer :: String           -- ^ URL
               -> String           -- ^ Bearer token
               -> LBS.ByteString   -- ^ payload
               -> IO (Either String LBS.ByteString)
postWithBearer url token payload = do
    result <- try $ do
        request <- parseRequest ("POST " ++ url)
        let authenticatedRequest = setRequestHeaders 
                [ ("Authorization", BS8.pack $ "Bearer " ++ token)
                , ("Content-Type", "application/json")
                ] request
        let finalRequest = setRequestBodyLBS payload authenticatedRequest
        
        response <- httpLBS finalRequest
        let status = statusCode $ getResponseStatus response
        
        if status >= 200 && status < 300
            then return $ Right $ getResponseBody response
            else return $ Left $ "HTTP Error: " ++ show status
    
    case result of
        Left (ex :: SomeException) -> return $ Left $ "Request failed: " ++ show ex
        Right res -> return res

sendRaw :: LBS.ByteString -> IO (Either String LBS.ByteString)
sendRaw = postWithBearer communicateUrl bearerToken

sendText :: T.Text -> IO (Either String LBS.ByteString)
sendText msg = case unparse msg of
  Nothing -> return(Left "Unrecognized character in input.")
  Just payload -> sendRaw payload
