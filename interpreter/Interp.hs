module Interp
( Expr
, interp
, num, str
, add, cat
) where

data Expr a
  = Num Int
  | Str String
  | Op BindOp (Expr a) (Expr a)
  deriving (Show)

data BindOp = Add | Concat deriving (Show)

num :: Int -> Expr Int
num = Num

str :: String -> Expr String
str = Str

add :: Expr Int -> Expr Int -> Expr Int
add = Op Add

cat :: Expr String -> Expr String -> Expr String
cat = Op Concat

interp :: Expr a -> Expr a
interp x@(Num _) = x
interp x@(Str _) = x
interp (Op Add a b) = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat a b) = Str (i a ++ i b)
  where i x = case interp x of Str a -> a
