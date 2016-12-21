import Data.Char
import Data.Function
import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set

doubleMe x = x + x

doubleUs x y =  doubleMe x +  doubleMe y

doubleSmallNumber number = if number > 100 then number else doubleMe number

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeUppercase str = [ c | c <- str, c `elem` ['a'..'z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

circumference :: Float -> Float
circumference r = 2 * pi * r

head' :: [a] -> a
head' (x:xs) = x

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

divide :: Float -> Float -> Float
divide 0 _ = 0
divide _ 0 = 0
divide num1 num2
  | num1 < num2 = 0 
  | otherwise   = num1 / num2

calcBmi :: (RealFloat a) => a -> a -> a
calcBmi weight height = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= fat    = "Overweight"
  | otherwise     = "Obese"
  where bmi = calcBmi weight height
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [calc w h | (w, h) <- xs]
  where calc weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^2
  in  sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = 
  case xs of []    -> error "No head for empty lists!"
             (x:_) -> x

duplicate :: [Int] -> [Int]
duplicate xs = [twice |x <- xs, let twice = x + x]

--max' (x:y:xs) = if x > y then max' (x:xs) else max' (y:xs)
max' :: (Ord a) => [a] -> a
max' []  = error "Maximum of empty list"
max' [x] = x
max' (x:xs)
  | x > maxTail = x
  | otherwise   = maxTail
  where maxTail = max' xs

max'' :: (Ord a) => [a] -> a
max'' []     = error "Maximum of empty list"
max'' [x]    = x
max'' (x:xs) = max x (max'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | x == e    = True
  | otherwise = elem' e xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' []     = []
quickSort' (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quickSort' [x2 | x2 <- xs, x2 <= x]
        bigger  = quickSort' [x2 | x2 <- xs, x2 > x ]

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTenBy :: (Floating a) => a -> a
divideTenBy = (10/)

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

largestDivisibleBy :: (Integral a) => a -> a
largestDivisibleBy n = head(filter p [100000, 99999..])
  where p x = x `mod` n == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | odd n  = n : chain (n * 3 + 1)
  | even n = n : chain (n `div` 2)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15


sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
--sum'' xs = foldl (\acc x -> acc + x) 0 xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product'' :: (Num a) => [a] -> a
product'' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head''' :: [a] -> a
head''' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' _ _ []       = []
scanl'' f acc (x:xs) = res : scanl'' f res xs
  where res = f acc x

quickSort'' :: (Ord a) => [a] -> [a]
quickSort'' []     = []
quickSort'' (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quickSort'' . filter (<x)  $ xs
        bigger  = quickSort'' . filter (>=x) $ xs

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False $ tails haystack

words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

decode :: Int -> String -> String
decode shift = encode (negate shift)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldl (\acc (k,v) -> if k == key then Just v else acc) Nothing
