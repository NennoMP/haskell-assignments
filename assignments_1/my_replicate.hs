-- EXERCISE 1
-- v1: recursive
myReplicateR :: Int -> a -> [a]
myReplicateR n v
  | n == 0 = []
  | otherwise = v:myReplicateR (n-1) v
-- v2: combinators (concatMap)
myReplicateC :: Int -> a -> [a]
myReplicateC n v = concatMap (replicate n) [v]


-- tests
main :: IO ()
main = do
  print "-- TESTING v1 (CHARACTERS)"
  putStr(show (myReplicateR 3 "r") ++ "\n")
  putStr(show (myReplicateR 0 "r") ++ "\n")
  putStr(show (myReplicateR 3 "") ++ "\n")
  putStr "-- TESTING v1 (NUMBERS)"
  putStr(show (myReplicateR 0 1) ++ "\n")
  putStr(show (myReplicateR 3 1) ++ "\n\n")
  
  print "-- TESTING v2 (CHARACTERS)"
  putStr(show (myReplicateC 3 "c") ++ "\n")
  putStr(show (myReplicateC 0 "c") ++ "\n")
  putStr(show (myReplicateC 3 "") ++ "\n")
  print "-- TESTING v2 (NUMBERS)"
  putStr(show (myReplicateC 3 1) ++ "\n")
  putStr(show (myReplicateC 0 1) ++ "\n")