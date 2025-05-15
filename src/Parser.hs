{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Parser where

import           Control.Applicative (Alternative (..), optional)
import           Data.Bifunctor      (first)
import           Data.Foldable       (foldl')
import           Data.Text           (Text)
import           Expr                hiding (EQ, LT, MUL, PRINT, RETURN)
import           Prelude             hiding (EQ, LT)
import           Tokenizer           (Token (..))

newtype Parser a = Parser {runParser :: [Token] -> [(a, [Token])]}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\i -> [(a, i)])

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser ab) (Parser a) = Parser $ \i -> do
    (f, i') <- ab i
    (b, i'') <- a i'
    [(f b, i'')]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const [])

  -- just combine the two parsers since one will be nil
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser l) (Parser r) = Parser $ \i -> l i ++ r i

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = Parser $ \case
  (t : ts) | p t -> [(t, ts)]
  _t -> []

token :: Token -> Parser Token
token t = satisfy (== t)

pluck :: (Token -> Maybe a) -> Parser a
pluck f = Parser $ \case
  (t : ts) -> case f t of
    Just t' -> [(t', ts)]
    Nothing -> []
  _t -> []

parensed :: Parser a -> Parser a
parensed p = token LPAREN *> p <* token RPAREN

endsWithSemi :: Parser a -> Parser a
endsWithSemi p = p <* token SEMI

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))

opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
opsL sep p = liftA2 squash p (many (liftA2 (,) sep p))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

braced :: Parser a -> Parser [a]
braced p =
  token LBRACE *> sepBy1 p (token SEMI) <* token RBRACE

data ParseError
  = FailedParse
  | AmbiguousParse [(Stmt, [Token])]
  deriving (Show, Eq)

identifier :: Parser Identifier
identifier =
  pluck $ \case
    NAME n -> Just (Name n)
    _n -> Nothing

name :: Parser Text
name =
  pluck $ \case
    NAME n -> Just n
    _n -> Nothing

literal :: Parser Expr
literal = intLit <|> ident
  where
    intLit =
      pluck $ \case
        INTEGER i -> Just (IntL i)
        _i -> Nothing
    ident =
      pluck $ \case
        NAME s -> Just (IdentExpr (Name s))
        _s -> Nothing

semi :: Parser Token
semi = token SEMI

comma :: Parser Token
comma = token COMMA

eof :: Parser ()
eof = Parser $ \case
  [] -> [((), [])]
  _i -> []

stmt :: Parser Stmt
stmt =
  (ExprStmt <$> expr <* semi)
    <|> printStmt
    <|> ifStmt
    <|> whileStmt
    <|> varStmt
    <|> funcStmt
    <|> returnStmt
  where
    printStmt = PrintStmt <$> (token PRINT *> expr) <* semi
    varStmt = VarStmt <$> (token VAR *> identifier) <*> optional (token ASSIGN *> expr) <* semi
    blockStmt = BlockStmt <$> (token LBRACE *> some stmt <* token RBRACE)
    ifStmt =
      IfStmt
        <$> (token IF *> expr)
        <*> blockStmt
        <*> optional (token ELSE *> blockStmt)
    whileStmt =
      WhileStmt
        <$> (token WHILE *> expr)
        <*> blockStmt
    funcStmt =
      FuncStmt <$> (token FUNC *> name) <*> parensed (sepBy name comma) <*> blockStmt
    returnStmt =
      ReturnStmt <$> (token RETURN *> expr <* semi)

expr :: Parser Expr
expr =
  comparisonExpr
    <|> assignExpr
  where
    assignExpr = AssignExpr <$> identifier <*> (token ASSIGN *> expr)

comparisonExpr :: Parser Expr
comparisonExpr = opsL comparisonOp unaryExpr
  where
    comparisonOp =
      (BinExpr LessThan <$ token LT)
        <|> (BinExpr LessThanEqual <$ token LTE)
        <|> (BinExpr GreaterThan <$ token LT)
        <|> (BinExpr GreaterThanEqual <$ token LTE)
        <|> (BinExpr Eq <$ token EQ)
        <|> (BinExpr Neq <$ token NEQ)

unaryExpr :: Parser Expr
unaryExpr = NegExpr <$> (token SUB *> expr) <|> addSubExpr

addSubExpr :: Parser Expr
addSubExpr = opsL addSubOp mulDivExpr
  where
    addSubOp =
      (BinExpr Add <$ token PLUS)
        <|> (BinExpr Sub <$ token SUB)

mulDivExpr :: Parser Expr
mulDivExpr = opsL mulDivOp factorExpr
  where
    mulDivOp =
      (BinExpr Mul <$ token MUL)
        <|> (BinExpr Div <$ token DIV)

funCall :: Parser Expr
funCall = FunCall <$> name <*> parensed (sepBy expr comma)

factorExpr :: Parser Expr
factorExpr = literal <|> (Grouping <$> parensed expr) <|> funCall

parser :: [Token] -> Either ParseError Stmt
parser input = case runParser ((BlockStmt <$> some stmt) <* eof) input of
  []         -> Left FailedParse
  [(res, _)] -> Right res
  tooMany    -> Left (AmbiguousParse tooMany)
