{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parse
  ( EvalError(..)
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
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Parse.String (decodeString, encodeString)
import Parse.Int (decodeInt)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Translator.Runtime
import Utils (toString)

-- Expression type for deferred evaluation with environment
data Expression
  = EConst Value
  | EVar Int  -- Variables are indexed by integers
  | EUnary (Value -> Value) Expression
  | EBinary (Value -> Value -> Value) Expression Expression
  | EIfThenElse Expression Expression Expression
  | ELambda Int Expression  -- Lambda parameter is an integer index
  | EApply Expression Expression

-- Environment for variable bindings (indexed by integers)
type Env = IntMap Value

-- Better error handling
data EvalError
  = TypeError String
  | ValueError
  | ParseError ParseError
  | DecodeError
  | RuntimeError String
  | UnboundVariable Int
  deriving (Eq, Show)

data ParseError
  = UnknownPrefix Char
  | UnknownOperator String
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

-- Helper to lift Maybe to Parser
maybeParse :: Maybe a -> Parser a
maybeParse = maybe (failParse InvalidBody) return

-- Parsing functions
tokenize :: LBS.ByteString -> [String]
tokenize = words . toString

-- Parse unary operations
parseUnary :: (Value -> Value) -> Parser Expression
parseUnary op = EUnary op <$> parseExpression

-- Parse binary operations
parseBinary :: (Value -> Value -> Value) -> Parser Expression
parseBinary op = EBinary op <$> parseExpression <*> parseExpression

-- Parse if-then-else
parseIfThenElse :: Parser Expression
parseIfThenElse = EIfThenElse <$> parseExpression <*> parseExpression <*> parseExpression

-- Parse lambda with integer parameter
parseLambda :: Int -> Parser Expression
parseLambda paramIdx = ELambda paramIdx <$> parseExpression

-- Parse function application  
parseApplication :: Parser Expression
parseApplication = EApply <$> parseExpression <*> parseExpression

-- Main expression parser
parseExpression :: Parser Expression
parseExpression = do
  token <- parseToken
  case token of
    "" -> failParse EmptyToken
    "T" -> return $ EConst $ mkBool True
    "F" -> return $ EConst $ mkBool False
    ('I':body) -> maybeParse $ EConst . mkInt <$> decodeInt body
    ('S':body) -> maybeParse $ EConst . mkString . T.unpack <$> decodeString body
    "U-" -> parseUnary negateVal
    "U!" -> parseUnary notVal
    "U#" -> parseUnary strToInt
    "U$" -> parseUnary intToStr
    "B+" -> parseBinary addVal
    "B-" -> parseBinary subVal
    "B*" -> parseBinary mulVal
    "B/" -> parseBinary quotVal
    "B%" -> parseBinary remVal
    "B=" -> parseBinary eqVal
    "B<" -> parseBinary ltVal
    "B>" -> parseBinary gtVal
    "B|" -> parseBinary orVal
    "B&" -> parseBinary andVal
    "B." -> parseBinary concatVal
    "BT" -> parseBinary takeVal
    "BD" -> parseBinary dropVal
    "B$" -> parseApplication
    "B!" -> parseApplication  -- Strict application, but we'll treat it the same
    "B~" -> parseApplication
    "?" -> parseIfThenElse
    ('L':body) -> maybeParse (decodeInt body) >>= parseLambda
    ('v':body) -> maybeParse $ EVar <$> decodeInt body
    ('U':op) -> failParse $ UnknownOperator $ "U" ++ op
    ('B':op) -> failParse $ UnknownOperator $ "B" ++ op
    (pfx:_) -> failParse $ UnknownPrefix pfx

-- Evaluation with environment
evaluateWithEnv :: Env -> Expression -> Either EvalError Value
evaluateWithEnv env expr = case expr of
  EConst v -> Right v
  EVar idx -> case IntMap.lookup idx env of
    Just v -> Right v
    Nothing -> Left $ UnboundVariable idx
  EUnary op e -> do
    v <- evaluateWithEnv env e
    Right $ op v
  EBinary op e1 e2 -> do
    v1 <- evaluateWithEnv env e1
    v2 <- evaluateWithEnv env e2
    Right $ op v1 v2
  EIfThenElse cond t f -> do
    condVal <- evaluateWithEnv env cond
    tVal <- evaluateWithEnv env t
    fVal <- evaluateWithEnv env f
    Right $ ifThenElse condVal tVal fVal
  ELambda paramIdx body ->
    -- Create a closure that captures the current environment
    Right $ mkFunc $ \arg ->
      case evaluateWithEnv (IntMap.insert paramIdx arg env) body of
        Right v -> v
        Left err -> error $ "Runtime error in lambda: " ++ show err
  EApply func argExpr -> do
    funcVal <- evaluateWithEnv env func
    argVal <- evaluateWithEnv env argExpr
    Right $ applyFunc funcVal argVal

-- Top-level evaluation starts with empty environment
evaluate :: Expression -> Either EvalError Value
evaluate = evaluateWithEnv IntMap.empty

-- Display logic using Runtime's extractResult
display :: Value -> Either EvalError T.Text
display val = Right $ T.pack $ extractResult val

-- Main parsing function
parse :: LBS.ByteString -> Either EvalError T.Text
parse raw =
  let initialState = tokenize raw
  in case runParser parseExpression initialState of
    Right (expr, _) -> do
      val <- evaluate expr
      display val
    Left err -> Left $ ParseError err

unparse :: T.Text -> Maybe LBS.ByteString
unparse msg = LBS.cons 83 <$> encodeString msg -- ASCII S
