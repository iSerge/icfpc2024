module Term (UOp (..), BOp (..), Term (..), toICFP) where

import Base94
import Text.Read

data UOp = Neg | Not | Str2Int | Int2Str
  deriving (Eq)

instance Show UOp where
  show Neg = "-"
  show Not = "!"
  show Str2Int = "#"
  show Int2Str = "$"

instance Read UOp where
  readPrec = do
    c <- get
    case c of
      '-' -> return Neg
      '!' -> return Not
      '#' -> return Str2Int
      '$' -> return Int2Str
      _ -> pfail

data BOp = Add | Sub | Mul | Div | Mod | LE | GT | EQ | Or | And | Concat | Take | Drop | Apply
  deriving (Eq)

instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show LE = "<"
  show Term.GT = ">"
  show Term.EQ = "="
  show Or = "|"
  show And = "&"
  show Concat = "."
  show Take = "T"
  show Drop = "D"
  show Apply = "$"

instance Read BOp where
  readPrec = do
    c <- get
    case c of
      '+' -> return Add
      '-' -> return Sub
      '*' -> return Mul
      '/' -> return Div
      '%' -> return Mod
      '<' -> return LE
      '>' -> return Term.GT
      '=' -> return Term.EQ
      '|' -> return Or
      '&' -> return And
      '.' -> return Concat
      'T' -> return Take
      'D' -> return Drop
      '$' -> return Apply
      _ -> pfail

data Term
  = T
  | F
  | I Int
  | S String
  | U UOp Term
  | B BOp Term Term
  | If Term Term Term
  | L Int Term
  | V Int
  deriving (Eq)

instance Show Term where
  show T = "True"
  show F = "False"
  show (I n) = show n
  show (S s) = "\n" ++ s ++ "\n" -- show s
  show (U op t) = show op ++ show t
  show (B op t1 t2) = "(" ++ show t1 ++ show op ++ show t2 ++ ")"
  show (If c t f) = "if (" ++ show c ++ ") then {" ++ show t ++ "} else {" ++ show f ++ "}"
  show (L n t) = "\\" ++ show n ++ " -> (" ++ show t ++ ")"
  show (V n) = "v" ++ show n

toICFP :: Term -> IcfpString
toICFP T = "T"
toICFP F = "F"
toICFP (I n) = "I" ++ n2icfp n
toICFP (S s) = "S" ++ s2icfp s
toICFP (U op t) = "U" ++ show op ++ " " ++ toICFP t
toICFP (B op t1 t2) = "B" ++ show op ++ " " ++ toICFP t1 ++ " " ++ toICFP t2
toICFP (If c t f) = "? " ++ toICFP c ++ " " ++ toICFP t ++ " " ++ toICFP f
toICFP (L n t) = "L" ++ n2icfp n ++ " " ++ toICFP t
toICFP (V n) = "v" ++ n2icfp n
