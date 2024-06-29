{-# LANGUAGE OverloadedStrings #-}

module Parser (icfpParser, parseIcfp, parseExpr) where

import Base94 (icfp2n, icfp2s, icfpCodes)
import Data.Functor as F
import Data.List.Utils
import Data.Text (Text)
import qualified Data.Text as Text
import Term
import Text.Parsec

encoding :: Parsec String () String
encoding = many1 $ oneOf icfpCodes

true :: Parsec String () Term
true = char 'T' $> T

false :: Parsec String () Term
false = char 'F' $> F

int :: Parsec String () Term
int = do
  _ <- char 'I'
  I . icfp2n <$> encoding

str :: Parsec String () Term
str = do
  _ <- char 'S'
  S . icfp2s <$> encoding

var :: Parsec String () Term
var = do
  _ <- char 'v'
  V . icfp2n <$> encoding

unary :: Parsec String () Term
unary = do
  _ <- char 'U'
  c <- oneOf "-!#$"
  _ <- char ' '
  U (read [c] :: UOp) <$> icfp

binary :: Parsec String () Term
binary = do
  _ <- char 'B'
  c <- oneOf "+-*/%<>=|&.TD$"
  _ <- char ' '
  t1 <- icfp
  _ <- char ' '
  B (read [c] :: BOp) t1 <$> icfp

lambda :: Parsec String () Term
lambda = do
  _ <- char 'L'
  s <- encoding
  _ <- char ' '
  L (icfp2n s) <$> icfp

condition :: Parsec String () Term
condition = do
  _ <- char '?'
  _ <- char ' '
  c <- icfp
  _ <- char ' '
  t <- icfp
  _ <- char ' '
  If c t <$> icfp

icfp :: Parsec String () Term
icfp = choice [true, false, int, str, var, unary, binary, lambda, condition]

icfpParser :: Parsec String () [Term]
icfpParser = sepBy1 icfp (many1 $ char ' ')

parseExpr :: String -> Term
parseExpr s = case parse icfp "Test expr" s of
  Right t -> t
  Left e -> S $ "Parse error: " ++ show e

parseIcfp :: Text -> Text
parseIcfp x = case parse icfpParser "Comm channel" (Text.unpack x) of
  Right n -> Text.pack $ join "\n" $ map show n
  Left e -> Text.pack $ "Comm error: " ++ show e ++ "\n Full input is:\n" ++ show x
