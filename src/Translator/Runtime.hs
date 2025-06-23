{-# LANGUAGE RankNTypes #-}

module Translator.Runtime 
    ( Value(..)
    , mkInt, mkBool, mkString, mkFunc
    , applyFunc
    , negateVal, notVal, strToInt, intToStr
    , quotVal, remVal, eqVal, orVal, andVal
    , concatVal, takeVal, dropVal
    , addVal, subVal, mulVal
    , ltVal, gtVal
    , ifThenElse
    , extractResult
    , reallyEncodeString
    , reallyDecodeInt
    , encodeInt
    , reallyDecodeString
    ) where

import Parse.String (encodeString, decodeString)
import Parse.Int (encodeInt, decodeInt)
import Utils (toString)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- Universal value type that can represent all values including self-referential functions
data Value = VInt Int
           | VBool Bool  
           | VString String
           | VFunc (Value -> Value)

-- Smart constructors
mkInt :: Int -> Value
mkInt = VInt

mkBool :: Bool -> Value
mkBool = VBool

mkString :: String -> Value
mkString = VString

mkFunc :: (Value -> Value) -> Value
mkFunc = VFunc

-- Function application
applyFunc :: Value -> Value -> Value
applyFunc (VFunc f) v = f v
applyFunc _ _ = error "Type error: applying non-function"

-- Unary operations
negateVal :: Value -> Value
negateVal (VInt n) = VInt (negate n)
negateVal _ = error "Type error: negate expects Int"

notVal :: Value -> Value
notVal (VBool b) = VBool (not b)
notVal _ = error "Type error: not expects Bool"

strToInt :: Value -> Value
strToInt (VString s) = VInt (reallyDecodeInt $ reallyEncodeString s)
strToInt _ = error "Type error: strToInt expects String"

intToStr :: Value -> Value
intToStr (VInt n) = VString (reallyDecodeString $ encodeInt n)
intToStr _ = error "Type error: intToStr expects Int"

-- Binary operations
quotVal :: Value -> Value -> Value
quotVal (VInt a) (VInt b) = VInt (a `quot` b)
quotVal _ _ = error "Type error: quot expects Int arguments"

remVal :: Value -> Value -> Value
remVal (VInt a) (VInt b) = VInt (a `rem` b)
remVal _ _ = error "Type error: rem expects Int arguments"

eqVal :: Value -> Value -> Value
eqVal (VInt a) (VInt b) = VBool (a == b)
eqVal (VBool a) (VBool b) = VBool (a == b)
eqVal (VString a) (VString b) = VBool (a == b)
eqVal _ _ = VBool False

orVal :: Value -> Value -> Value
orVal (VBool a) (VBool b) = VBool (a || b)
orVal _ _ = error "Type error: or expects Bool arguments"

andVal :: Value -> Value -> Value
andVal (VBool a) (VBool b) = VBool (a && b)
andVal _ _ = error "Type error: and expects Bool arguments"

concatVal :: Value -> Value -> Value
concatVal (VString a) (VString b) = VString (a ++ b)
concatVal _ _ = error "Type error: concat expects String arguments"

takeVal :: Value -> Value -> Value
takeVal (VInt n) (VString s) = VString (take n s)
takeVal _ _ = error "Type error: take expects Int and String"

dropVal :: Value -> Value -> Value
dropVal (VInt n) (VString s) = VString (drop n s)
dropVal _ _ = error "Type error: drop expects Int and String"

addVal :: Value -> Value -> Value
addVal (VInt a) (VInt b) = VInt (a + b)
addVal _ _ = error "Type error: add expects Int arguments"

subVal :: Value -> Value -> Value
subVal (VInt a) (VInt b) = VInt (a - b)
subVal _ _ = error "Type error: sub expects Int arguments"

mulVal :: Value -> Value -> Value
mulVal (VInt a) (VInt b) = VInt (a * b)
mulVal _ _ = error "Type error: mul expects Int arguments"

ltVal :: Value -> Value -> Value
ltVal (VInt a) (VInt b) = VBool (a < b)
ltVal _ _ = error "Type error: lt expects Int arguments"

gtVal :: Value -> Value -> Value
gtVal (VInt a) (VInt b) = VBool (a > b)
gtVal _ _ = error "Type error: gt expects Int arguments"

-- Helper for if-then-else
ifThenElse :: Value -> Value -> Value -> Value
ifThenElse (VBool True) t _ = t
ifThenElse (VBool False) _ f = f
ifThenElse _ _ _ = error "Type error: if expects Bool condition"

-- Extract result for display
extractResult :: Value -> String
extractResult (VInt n) = show n
extractResult (VBool b) = show b
extractResult (VString s) = s
extractResult (VFunc _) = "<function>"

-- Export ifThenElse
instance Show Value where
  show = extractResult

-- Legacy functions from original Runtime
reallyEncodeString :: String -> String
reallyEncodeString s = maybe "Encoding Error" toString (encodeString $ T.pack s)

reallyDecodeString :: String -> String
reallyDecodeString s = maybe "Decoding Error" T.unpack (decodeString s)

reallyDecodeInt :: String -> Int
reallyDecodeInt s = fromMaybe (-1337) $ decodeInt s

