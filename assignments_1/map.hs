-- EXERCISE 8
-- map implementation with foldl (base case [] included in combinators)
myMap :: (a -> b) -> [a] -> [b]
myMap f l =  foldl (\xs x -> xs ++ (f x):[]) [] l

-- tests
main :: IO ()
main = do
  print "-- TESTING"
  putStr(show (myMap (+1) []) ++ "\n")
  putStr(show (myMap (+1) [1, 2, 3, 4, 5]) ++ "\n")