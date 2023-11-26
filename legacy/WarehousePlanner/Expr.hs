{-# LANGUAGE DeriveFunctor #-}
module WarehousePlanner.Expr
( Expr(..)
, parseExpr
, parseExpr'
, evalExpr
) where
import ClassyPrelude hiding(readFile)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import Text.Read(readMaybe)

data Expr extra = AddE (Expr extra) (Expr extra)
          | SubE (Expr extra) (Expr extra)
          | MulE (Expr extra) (Expr extra)
          | DivE (Expr extra) (Expr extra)
          | MinE (Expr extra) (Expr extra)
          | MaxE (Expr extra) (Expr extra)
          | ValE Double
          | ExtraE extra --
     deriving (Show, Functor)
          
          
-- * Parsing
parseExpr :: Text -> (Expr Text)
parseExpr s =  case P.parse (parseExpr' <* P.eof) (unpack s) s of
  Left err -> error (show err)
  Right expr -> expr

parseExpr' :: P.Parser (Expr Text)
parseExpr'  = (P.try (parseMMOp ))
                    <|> parseTerminalExpr 

parseTerminalExpr :: P.Parser (Expr Text)
parseTerminalExpr  = parseVal <|> parseExtra <|> parseGroup 
parseVal :: P.Parser (Expr Text)
parseVal = do
      n <- P.many1 P.digit
      f <- P.option "" ((:) <$> P.char '.' <*> P.many1 P.digit)
      let s =  n ++ f
      return $ case readMaybe s of
                  Nothing -> error $ "Can't parse [" ++ s ++ "]"
                  Just v -> ValE v

parseGroup :: P.Parser (Expr Text)
parseGroup  = do
  _ <- P.char '('
  P.spaces
  e <- parseExpr' 
  P.spaces
  _ <- P.char ')'
  return e

parseMulOp :: P.ParsecT Text () Identity (Expr Text)
parseMulOp  = P.try p <|> parseTerminalExpr  where
  p = do
    e1 <- parseTerminalExpr 
    P.spaces
    op <- P.oneOf "*/"
    P.spaces
    e2 <- parseTerminalExpr 
    let c = case op of
            '*' -> MulE
            '/' -> DivE
            _ -> error "should not happen"
    return $ c e1 e2

parseMMOp :: P.ParsecT Text () Identity (Expr Text)
parseMMOp  = P.try p <|> parseAddOp  where
  p = do
    e1 <- parseAddOp 
    P.spaces
    op <- P.oneOf "|&"
    P.spaces
    e2 <- parseAddOp 
    let c = case op of
            '&' -> MinE
            '|' -> MaxE
            _ -> error "should not happen"
    return $ c e1 e2

parseAddOp :: P.ParsecT Text () Identity (Expr Text)
parseAddOp = P.try p <|> parseMulOp where
  p = do
    e1 <- parseMulOp 
    P.spaces
    op <- P.oneOf "+-"
    P.spaces
    e2 <- parseMulOp 
    let c = case op of
            '+' -> AddE
            '-' -> SubE
            _ -> error "should not happen"
    return $ c e1 e2

parseExtra :: P.Parser (Expr Text)
parseExtra = do
  _ <- P.char '{'
  extra <- P.many (P.noneOf "}")
  _ <- P.char '}'
  return $ ExtraE (pack extra)



-- parseRef :: P.Parser (Expr (RefE 
-- parseRef accessor = do
--   _ <- P.char '{'
--   extra <- P.many (P.noneOf ":}") --  (P.alphaNum <|> P.oneOf ".+-%_\\")
--   acc <- P.option accessor $ P.char ':' *> parseAccessor
--   _ <- P.char '}'
--   return $ ExtraE (pack extra) acc
-- * Eval
evalExpr :: Expr Double -> Double
evalExpr (AddE e1 e2) = evalOperator (+) e1 e2
evalExpr (SubE e1 e2) = evalOperator (-) e1 e2
evalExpr (MulE e1 e2) = evalOperator (*) e1 e2
evalExpr (DivE e1 e2) = evalOperator (/) e1 e2
evalExpr (MinE e1 e2) = evalOperator min e1 e2
evalExpr (MaxE e1 e2) = evalOperator max e1 e2
evalExpr (ValE v) = v
evalExpr (ExtraE v) = v

evalOperator op e1 e2 = evalExpr e1 `op` evalExpr e2
