data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

tellCar :: Car -> String
tellCar (Car company model year) = "This " ++ company ++ " " ++ model ++ " was made in " ++ show year
--tellCar car = "This " ++ car.company ++ " " ++ car.model ++ " was made in " ++ show car.year


--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x <  a = Node a (treeInsert x left) right
  | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x <  a = treeElem x left
  | x >  a = treeElem x right

instance Functor Tree where
  fmap f EmptyTree           = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
