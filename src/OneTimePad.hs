module OneTimePad (solve) where

solve :: String -> IO ()
solve input = do
  let prefix = head . lines $ input
  print prefix
