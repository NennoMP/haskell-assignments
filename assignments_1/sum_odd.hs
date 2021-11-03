-- EXERCISE 2
-- v1: recursive
sumOddR :: [Int] -> Int
sumOddR [] = 0
sumOddR (x:xs) = if (odd x) then x + (sumOddR xs) else sumOddR xs
-- v2: combinators (filter)
sumOddC :: [Int] -> Int
sumOddC l = sum (filter odd l)

 

-- tests
main :: IO ()
main = do
  print "-- TESTING v1"
  putStr(show (sumOddR []) ++ "\n")
  putStr(show (sumOddR [2, 4, 6, 8, 10]) ++ "\n")
  putStr(show (sumOddR [1, 3, 5, 7, 9]) ++ "\n")
  putStr(show (sumOddR [1, 2, 3, 4, 5]) ++ "\n\n")
  print "-- TESTING v2"
  putStr(show (sumOddC []) ++ "\n")
  putStr(show (sumOddC [2, 4, 6, 8, 10]) ++ "\n")
  putStr(show (sumOddC [1, 3, 5, 7, 9]) ++ "\n")
  putStr(show (sumOddC [1, 2, 3, 4, 5]) ++ "\n")