{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Communicator
import Parse
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  result <- sendRaw "S'%4}).$%8"
  case result of
    Left err -> putStrLn err
    Right response -> case parse response of
      Nothing -> putStrLn "Parse failed"
      Just parsed -> TIO.putStrLn parsed
