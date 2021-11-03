-- EXERCISE 3
-- utility
myReplicateR :: Int -> a -> [a]
myReplicateR n v
  | n == 0 = []
  | otherwise = v:myReplicateR (n-1) v
-- v1: recursive
replR :: [a] -> Int -> [a]
replR [] _ = []
replR (x:xs) n = (myReplicateR n x) ++ replR xs n
-- v2: combinators (concatMap)
replC :: [a] -> Int -> [a]
replC l n = concatMap (replicate n) l

-- tests
main :: IO ()
main = do
  print "-- TESTING v1"
  putStr(show (replR ([]::[Int]) 3) ++ "\n")
  putStr(show (replR [1, 2, 3] 0) ++ "\n")
  putStr(show (replR [1, 2, 3] 3) ++ "\n")
  putStr(show (replR ["a", "b", "c"] 0) ++ "\n")
  putStr(show (replR ["a", "b", "c"] 3) ++ "\n\n")

  print "-- TESTING v2"
  putStr(show (replC ([]::[Int]) 1) ++ "\n")
  putStr(show (replC [1, 2, 3] 0) ++ "\n")
  putStr(show (replC [1, 2, 3] 3) ++ "\n")
  putStr(show (replC ["a", "b", "c"] 0) ++ "\n")
  putStr(show (replC ["a", "b", "c"] 3) ++ "\n")