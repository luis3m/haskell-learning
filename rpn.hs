import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl folding [] . words
  where folding (x:y:ys) "*" = (y * x):ys
        folding (x:y:ys) "+" = (y + x):ys
        folding (x:y:ys) "-" = (y - x):ys
        folding xs numberStr = read numberStr:xs

advanceSolveRPN :: String -> Float
advanceSolveRPN = head . foldl folding [] . words
  where folding (x:y:ys) "*" = (y * x):ys
        folding (x:y:ys) "+" = (y + x):ys
        folding (x:y:ys) "-" = (y - x):ys
        folding (x:y:ys) "/" = (y / x):ys
        folding (x:y:ys) "^" = (y ** x):ys
        folding xs "sum"     = [sum xs]
        folding xs numberStr = read numberStr:xs
