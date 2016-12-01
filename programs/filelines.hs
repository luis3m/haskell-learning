import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import Control.Exception.Base
import Prelude hiding (catch)

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fname:_) <- getArgs
  exists    <- doesFileExist fname
  if exists
    then do contents  <- readFile fname
            putStrLn $ "The file has " ++ show (length $ lines contents) ++ " lines!"
    else do putStrLn "The file doesn't exist!"

handler :: IOException -> IO ()
handler ex = putStrLn $ "Caught exception: " ++ show ex
