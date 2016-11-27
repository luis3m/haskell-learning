import Data.Char
import Data.Function
import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set

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
