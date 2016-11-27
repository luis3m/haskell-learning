import Data.Char

main = do
  _     <- putStrLn "What's your first name? "
  fname <- getLine
  _     <- putStrLn "What's your last name? "
  lname <- getLine
  let ufname = map toUpper fname
      ulname = map toUpper lname
  putStrLn $ "Hello " ++ ufname ++ ", your last name is " ++ ulname
