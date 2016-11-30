main = do
  content <- getContents
  putStr $ shortLinesOnly content

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      sLines   = filter (\line -> length line < 10) allLines
      result   = unlines sLines
  in result
