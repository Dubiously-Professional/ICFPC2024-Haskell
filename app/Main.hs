{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Communicator

main :: IO ()
main = (sendRaw "S'%4}).$%8") >>= print
