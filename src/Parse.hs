{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parse
  ( Expression(..)
  , Value (..)
  , EvalError(..)
  , ParseError(..)
  , parse
  , unparse
  , evaluate
  , display
  , (Parse.<|>)
  , tokenize
  , Parser
  , parseToken
  , failParse
  , runParser
  , toString
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Parse.String (decodeString, encodeString)

-- Basic expression taking a fixed number of arguments. Extend as you need it.
data Expression = Constant Value |
                  UnaryOp (Value -> Value) Expression |
                  BinaryOp (Value -> Value -> Value) Expression Expression

-- Add more value types as you need them. 
data Value
  = StringValue T.Text
  | IntValue Int
  | Error EvalError
  deriving (Eq, Show)

-- Better error handling
data EvalError
  = TypeError
  | ValueError
  | ParseError ParseError
  | DecodeError
  deriving (Eq, Show)

data ParseError
  = UnknownPrefix Char
  | InputUnderflow
  | EmptyToken
  | InvalidBody
  | SyntaxError String
  deriving (Eq, Show)

newtype Parser a = Parser { runParser :: [String] -> Either ParseError (a, [String]) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> 
    case p s of
      Right (a, s') -> Right (f a, s')
      Left err  -> Left err

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  Parser pf <*> Parser pa = Parser $ \s ->
    case pf s of
      Right (f, s1) -> case pa s1 of
        Right (a, s2) -> pure (f a, s2)
        Left err -> Left err
      Left err  -> Left err

instance Monad Parser where
  Parser p >>= f = Parser $ \s ->
    case p s of
      Right (a, s') -> runParser (f a) s'
      Left err  -> Left err

-- Basic parser combinators
parseToken :: Parser String
parseToken = Parser $
  \case
    [] -> Left InputUnderflow
    (t:ts) -> return (t, ts)

failParse :: ParseError -> Parser a
failParse err = Parser $ const $ Left err

(<|>) :: Parser a -> Parser a -> Parser a
Parser p1 <|> Parser p2 = Parser $ \s ->
  case p1 s of
    Left _ -> p2 s
    result -> result

-- Parsing functions
evaluate :: Expression -> Value
evaluate (Constant x) = x
evaluate (UnaryOp f arg) = f $ evaluate arg
evaluate (BinaryOp f arg1 arg2) = f (evaluate arg1) (evaluate arg2)

toString :: LBS.ByteString -> String
toString raw =  map (toEnum . fromIntegral) $ LBS.unpack raw

tokenize :: LBS.ByteString -> [String]
tokenize = words.toString

-- Helper method to build functions of values. Implement other helpers as you need them
lift :: (Int -> Int -> Int) -> Value -> Value -> Value
lift f (IntValue x) (IntValue y) = IntValue $ f x y
lift _ _ _ = Error TypeError

parseExpression :: Parser Expression
parseExpression = do
  token <- parseToken
  case token of
    "" -> failParse EmptyToken
    (pfx:sfx) -> case pfx of
      'S' -> return $ Constant $ makeString sfx
      -- Replace this with the way you parse unary operations.
      'U' -> UnaryOp id <$> parseExpression
        -- Replace this with the way you parse binary operations.
      'B' -> BinaryOp (lift (+)) <$> parseExpression <*> parseExpression
      _ -> failParse $ UnknownPrefix pfx

-- Implement other display logic if you need it here
display :: Value -> Either EvalError T.Text
display (StringValue t) = Right t
display (IntValue n) = Right $ T.pack $ show n
display (Error err) = Left err

makeString :: String -> Value
-- Better to have a specific error than TypeError in Value
makeString str = maybe (Error DecodeError) StringValue $ decodeString str

-- Main parsing function. Add logic to handle extra tokens if you need it.
parse :: LBS.ByteString -> Either EvalError T.Text
parse raw = 
  let initialState = tokenize raw
  in case runParser parseExpression initialState of
    Right (expr, _) -> display $ evaluate expr
    Left err        -> Left $ ParseError err

unparse :: T.Text -> Maybe LBS.ByteString
unparse msg = LBS.cons 83 <$> encodeString msg -- ASCII S
