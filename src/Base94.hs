module Base94 (IcfpString, n2icfp, icfp2n, s2icfp, icfp2s, icfpCodes) where

import Data.Char (chr, ord)

type IcfpString = String

n2icfp :: Int -> IcfpString
n2icfp = iter []
  where
    iter :: IcfpString -> Int -> IcfpString
    iter [] 0 = chr 33 : []
    iter s 0 = s
    iter s n = iter (chr ((n `mod` 94) + 33) : s) (n `div` 94)

icfp2n :: IcfpString -> Int
icfp2n = acc 0
  where
    acc n [] = n
    acc n (x : xs) = acc (n * 94 + (ord (x) - 33)) xs

icfpAscii :: String
icfpAscii = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

icfpCodes :: IcfpString
icfpCodes = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

indexOf :: String -> Char -> Int
indexOf = iter 0
  where
    iter _ [] _ = undefined
    iter n (s : _) c | s == c = n
    iter n (_ : sx) c = iter (n + 1) sx c

icfp2c :: Char -> Char
icfp2c c = icfpAscii !! (ord c - 33)

s2icfp :: String -> IcfpString
s2icfp = map (\c -> chr (indexOf icfpAscii c + 33))

icfp2s :: IcfpString -> String
icfp2s = map icfp2c
