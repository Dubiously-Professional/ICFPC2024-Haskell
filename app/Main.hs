{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Communicator
import System.IO
import Parse
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  msg <- getLine
  result <- sendText $ T.pack msg
  case result of
    Left err -> putStrLn err
    Right response -> case parse response of
      Nothing -> putStrLn "Parse failed"
      Just parsed -> TIO.putStrLn parsed
