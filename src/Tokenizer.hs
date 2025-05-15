{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Turn a string into a stream of Wab tokens
module Tokenizer where

import           Control.Applicative (Alternative (..))
import           Data.Bifunctor      (Bifunctor (first))
import           Data.Char           (isAlphaNum, isDigit, isLower, isSpace,
                                      isUpper)
import           Data.List           (foldl1')
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text, uncons)
import qualified Data.Text           as T
import           Prelude             hiding (EQ, GT, LT)

data Token
  = -- keywords
    VAR
  | PRINT
  | IF
  | ELSE
  | WHILE
  | FUNC
  | RETURN
  | -- names
    NAME Text
  | -- literals
    INTEGER Int
  | PLUS
  | MUL
  | SUB
  | DIV
  | LT
  | LTE
  | GT
  | GTE
  | EQ
  | NEQ
  | ASSIGN
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  deriving (Show, Eq)

data LexerError
  = -- we got a character we didn't know what to do with
    Unexpected Char
  | -- we got to the end too early
    UnexpectedEOF
  deriving (Eq, Show)

-- Create the right lex error when we encounter an unexpected string
unexpected :: Text -> LexerError
unexpected t = case uncons t of
  Just (t', _) -> Unexpected t'
  Nothing      -> UnexpectedEOF

newtype Lexer a = Lexer
  { runLexer :: Text -> Either LexerError (a, Text)
  }

instance Functor Lexer where
  fmap :: (a -> b) -> Lexer a -> Lexer b
  fmap f (Lexer a) = Lexer (fmap (first f) . a)

instance Applicative Lexer where
  pure a = Lexer (\i -> Right (a, i))
  (<*>) (Lexer ab) (Lexer a) = Lexer $ \i -> do
    (f, i') <- ab i
    (a', i'') <- a i'
    return (f a', i'')

instance Alternative Lexer where
  empty = Lexer $ Left . unexpected
  (<|>) (Lexer l) (Lexer r) = Lexer $ \i -> do
    case (l i, r i) of
      (res, Left _) -> res
      (Left _, res) -> res
      (Right (_, rest), Right (_, rest')) ->
        if T.length rest <= T.length rest'
          then l i
          else r i

satisfy :: (Char -> Bool) -> Lexer Char
satisfy f = Lexer $ \i -> case uncons i of
  (Just (c, cs)) | f c -> Right (c, cs)
  (Just (c, _))        -> Left $ Unexpected c
  Nothing              -> Left UnexpectedEOF

-- | Match a single character
char :: Char -> Lexer Char
char c = satisfy (== c)

-- | Match a string
string :: Text -> Lexer Text
string s = T.pack <$> traverse char (T.unpack s)

oneOf :: (Alternative f) => [f a] -> f a
oneOf = foldl1' (<|>)

-- | add the given b to a value in a functor
-- used to attach tokens to their parsed values.
with :: (Functor f) => b -> f a -> f (b, a)
with b = fmap (b,)

token :: Lexer (Token, Text)
token = keyword <|> operator <|> literal <|> name
  where
    keyword :: Lexer (Token, Text)
    keyword =
      oneOf
        [ VAR `with` string "var",
          PRINT `with` string "print",
          IF `with` string "if",
          ELSE `with` string "else",
          WHILE `with` string "while",
          FUNC `with` string "func",
          RETURN `with` string "return"
        ]
    operator :: Lexer (Token, Text)
    operator =
      oneOf
        [ LPAREN `with` string "(",
          RPAREN `with` string ")",
          PLUS `with` string "+",
          MUL `with` string "*",
          SUB `with` string "-",
          DIV `with` string "/",
          LT `with` string "<",
          LTE `with` string "<=",
          GT `with` string ">",
          GTE `with` string ">=",
          EQ `with` string "==",
          NEQ `with` string "!=",
          ASSIGN `with` string "=",
          SEMI `with` string ";",
          LBRACE `with` string "{",
          RBRACE `with` string "}",
          COMMA `with` string ","
        ]

    literal :: Lexer (Token, Text)
    literal = intLit
      where
        intLit :: Lexer (Token, Text)
        intLit = (\x -> (INTEGER (read x), T.pack x)) <$> some (satisfy isDigit)

    name :: Lexer (Token, Text)
    name = upperName <|> lowerName
      where
        continuesName :: Lexer Char
        continuesName = satisfy isAlphaNum <|> char '\''

        followedBy :: Lexer Char -> Lexer Char -> Lexer Text
        followedBy l1 l2 = liftA2 T.cons l1 (T.pack <$> many l2)

        upperName :: Lexer (Token, Text)
        upperName = fmap (\x -> (NAME x, x)) (satisfy isUpper `followedBy` continuesName)

        lowerName :: Lexer (Token, Text)
        lowerName = fmap (\x -> (NAME x, x)) (satisfy isLower `followedBy` continuesName)

data RawToken
  = BlankSpace String
  | Comment String
  | Newline
  | NormalToken Token Text
  deriving (Show)

toToken :: RawToken -> Maybe Token
toToken (NormalToken t _) = Just t
toToken _                 = Nothing

rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> fmap (uncurry NormalToken) token)
  where
    whitespace = blankspace <|> newline
    blankspace = BlankSpace <$> some (satisfy (\x -> isSpace x && x /= '\n'))
    comment = Comment <$> (string "//" *> many (satisfy (/= '\n')))
    newline = Newline <$ char '\n'

lexer :: Text -> Either LexerError [Token]
lexer input = runLexer rawLexer input >>= (return . mapMaybe toToken . fst)
