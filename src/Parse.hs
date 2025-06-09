module Parse (Expression, Value, parse, unparse, evaluate, display) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Parse.String (decodeString, encodeString)

-- Basic expression taking a fixed number of arguments. Extend as you need it.
data Expression = Constant Value |
                  UnaryOp (Value -> Value) Expression |
                  BinaryOp (Value -> Value -> Value) Expression Expression |
                  ParseError

-- Add more value types as you need them. 
data Value = StringValue T.Text | IntValue Int | TypeError

evaluate :: Expression -> Value
evaluate (Constant x) = x
evaluate (UnaryOp f arg) = f $ evaluate arg
evaluate (BinaryOp f arg1 arg2) = f (evaluate arg1) (evaluate arg2)
evaluate ParseError = TypeError

tokenize :: LBS.ByteString -> [String]
tokenize raw = words $ map (toEnum . fromIntegral) $ LBS.unpack raw

-- Helper method to build functions of values. Implement other helpers as you need them
lift :: (Int -> Int -> Int) -> Value -> Value -> Value
lift f (IntValue x) (IntValue y) = IntValue $ f x y
lift _ _ _ = TypeError

translate :: [String] -> (Expression, [String])
translate [] = (ParseError, [])
translate (top:rest) = case top of
  "" -> (ParseError, [])
  (pfx:sfx) -> case pfx of
    'S' -> (Constant $ makeString sfx, rest)
    -- Replace this with the way you parse unary operations.
    'U' -> let (arg1, rest1) = translate rest in
             (UnaryOp id arg1, rest1)
    -- Replace this with the way you parse binary operations.
    'B' -> let (arg1, rest1) = translate rest
               (arg2, rest2) = translate rest1 in
             (BinaryOp (lift (+)) arg1 arg2, rest2)
    _ -> (ParseError, [])

-- Implement other display logic if you need it here
display :: Value -> Maybe T.Text
display (StringValue t) = Just t
display _ = Nothing

-- Add logic to handle extra tokens if you need it
parse :: LBS.ByteString -> Maybe T.Text
parse raw = let (expr, _) = translate $ tokenize raw in
  display $ evaluate expr

makeString :: String -> Value
makeString str = case decodeString str of
  Just s -> StringValue s
  _      -> TypeError

unparse :: T.Text -> Maybe LBS.ByteString
unparse msg = LBS.cons 83 <$> encodeString msg -- ASCII S
