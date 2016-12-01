import System.Random
import Control.Monad(when)

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (rand, nGen) = randomR (1,10) gen :: (Int, StdGen)
  _      <- putStr "Which number in the range 1-10 am I thinking of? "
  numStr <- getLine
  when (not $ null numStr) $ do
    let num = read numStr
    if rand == num
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show rand
    askForNumber nGen


askForNumber' :: StdGen -> [Int] -> IO ()
askForNumber' gen xs = do
  let (rand, nGen) = randomR (1,10) gen :: (Int, StdGen)
  if null xs
    then putStr "Which number in the range 1-10 am I thinking of? "
    else putStr $ (show $ length xs) ++ " attemps. Try again: "
  numStr <- getLine
  let num = read numStr
  if num /= rand
    then askForNumber' gen $ xs ++ [num]
    else do
      putStr $ "Correct! You tried " ++ (show $ xs ++ [num]) ++ ", continue playing (y/n)? "
      char <- getLine
      when (char == "y") (askForNumber' nGen [])
