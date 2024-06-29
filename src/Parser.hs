{-# LANGUAGE OverloadedStrings #-}

module Parser (icfpParser, parseIcfp) where

import Base94(icfpCodes, icfp2n, icfp2s)
import Term
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List.Utils

encoding :: Parsec String () String
encoding = many1 $ oneOf icfpCodes

true :: Parsec String () Term
true = char 'T' *> return T

false :: Parsec String () Term
false = char 'F' *> return F

int :: Parsec String () Term
int = do
     _ <- char 'I'
     s <- encoding 
     return $ I (icfp2n s)

str :: Parsec String () Term
str = do
     _ <- char 'S'
     s <- encoding
     return $ S (icfp2s s)

var :: Parsec String () Term
var = do
     _ <- char 'v'
     s <- encoding
     return $ V (icfp2n s)

unary :: Parsec String () Term
unary = do
     _ <- char 'U'
     c <- oneOf "-!#$"
     _ <- char ' '
     t <- icfp
     return $ U (read [c]::UOp) t

binary :: Parsec String () Term
binary = do
     _ <- char 'B'
     c <- oneOf "+-*/%<>=|&.TD$" 
     _ <- char ' '
     t1 <- icfp
     _ <- char ' '
     t2 <- icfp
     return $ B (read [c]::BOp) t1 t2

lambda :: Parsec String () Term
lambda = do
     _ <- char 'L'
     s <- encoding
     _ <- char ' '
     t <- icfp
     return $ L (icfp2n s) t

condition :: Parsec String () Term
condition = do
     _ <- char '?'
     _ <- char ' '
     c <- icfp
     _ <- char ' '
     t <- icfp
     _ <- char ' '
     f <- icfp
     return $ If c t f

icfp :: Parsec String () Term
icfp = choice [true, false, int, str, var, unary, binary, lambda, condition]

icfpParser :: Parsec String () [Term]
icfpParser = sepBy1 icfp (many1 $ char ' ')

parseIcfp :: Text -> Text
parseIcfp x = case parse icfpParser "Comm channel" (Text.unpack x) of
  Right n -> Text.pack $ join "\n" $ map show n
  Left e -> Text.pack $ "Comm error: " ++ show e ++ "\n Full input is:\n" ++ show x
