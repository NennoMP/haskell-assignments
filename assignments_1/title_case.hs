import Data.Char
-- EXERCISE 6
-- utility
firstToUpper :: String -> String
firstToUpper [] = []
firstToUpper (x:xs) = (toUpper x) : xs
-- v1 : recursion
titlecaseR :: String -> String
titlecaseR ls = unwords $ titlecaseAuxR (words ls) where
  titlecaseAuxR ls2 = case ls2 of
                           [] -> []
                           (head:tail) -> (firstToUpper head) : titlecaseAuxR tail
-- v2 : combinators
titlecaseC :: String -> String
titlecaseC ls = unwords $ titlecaseAuxC (words ls) where
  titlecaseAuxC ls2 = case ls2 of
                           [] -> []
                           ss -> map (firstToUpper) ss

-- tests
main :: IO ()
main = do
  print "-- TESTING v1"
  putStr(show (titlecaseR "") ++ "\n")
  putStr(show (titlecaseR "hat") ++ "\n")
  putStr(show (titlecaseR "my red hat") ++ "\n\n")
  print "-- TESTING v2"
  putStr(show (titlecaseC "") ++ "\n")
  putStr(show (titlecaseC "hat") ++ "\n")
  putStr(show (titlecaseC "my red hat") ++ "\n\n")