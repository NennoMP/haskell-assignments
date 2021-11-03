-- EXERCISE 4
-- utility
lsum [] = 0
lsum (x:xs) = x + sum(xs)
-- v1: recursive
totalLengthR :: [String] -> Int
totalLengthR [] = 0
totalLengthR (x:xs)
  | (head x) == 'a' = length x + totalLengthR xs
  | otherwise = 0 + totalLengthR xs
-- v2: combinators (map)
totalLengthC :: [String] -> Int
totalLengthC [] = 0
totalLengthC l = lsum (map (length) (filter (\x -> (head x) == 'a') l))


-- tests
main :: IO ()
main = do
  print "-- TESTING v1"
  putStr(show (totalLengthR []) ++ "\n")
  putStr(show (totalLengthR ["a"]) ++ "\n")
  putStr(show (totalLengthR ["apple", "banana", "aaa"]) ++ "\n\n")

  print "-- TESTING v2"
  putStr(show (totalLengthC []) ++ "\n")
  putStr(show (totalLengthC ["a"]) ++ "\n")
  putStr(show (totalLengthC ["apple", "banana", "aaa"]) ++ "\n")