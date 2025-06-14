module TranslatorApp (main) where

import Communicator
import System.IO
import Translator
import qualified Data.Text as T

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  msg <- getLine
  result <- sendText $ T.pack msg
  case result of
    Left err -> putStrLn err
    Right response -> 
      case translate response of
        Nothing -> putStrLn $ "Translation error"
        Just haskell -> putStrLn haskell