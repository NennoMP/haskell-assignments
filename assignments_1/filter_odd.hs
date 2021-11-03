-- EXERCISE 5
-- v1.1 : recursive
filterOddR1 :: [a] -> [a]
filterOddR1 l = filterOddRAux l 0 where 
    filterOddRAux l i = case(l, i) of 
                            ([],_) -> []
                            (x:xs,i) -> if (even i) 
                                            then x:filterOddRAux xs (i+1) 
                                            else filterOddRAux xs (i+1)
-- v1.2 : recursive                                           
filterOddR2 :: [a] -> [a]
filterOddR2 l = [l!!i | i<-[0..length l - 1], even i]
-- v1.3 : recursive
filterOddR3 :: [a] -> [a]
filterOddR3 (x1:x2:xs) = x1:filterOddR3 xs
filterOddR3 x1 = x1
-- v2 : combinators
filterOddC :: [a] -> [a]
filterOddC l = map fst $ filter (even.snd) indexed where -- zip -> [el,pos], filter even 2nd elements, take 1st
    indexed = zip l [0..]


main :: IO ()
main = do
  print "-- TESTING v1.1"
  putStr(show (filterOddR1 ([]::[Int])) ++ "\n")
  putStr(show (filterOddR1 [1, 2, 3, 4, 5]) ++ "\n")
  putStr(show (filterOddR1 ["a", "b", "c", "d", "e"]) ++ "\n")
  print "-- TESTING v1.2"
  putStr(show (filterOddR2 ([]::[Int])) ++ "\n")
  putStr(show (filterOddR2 [1, 2, 3, 4, 5]) ++ "\n")
  putStr(show (filterOddR2 ["a", "b", "c", "d", "e"]) ++ "\n")
  print "-- TESTING v1.3"
  putStr(show (filterOddR3 ([]::[Int])) ++ "\n")
  putStr(show (filterOddR3 [1, 2, 3, 4, 5]) ++ "\n")
  putStr(show (filterOddR3 ["a", "b", "c", "d", "e"]) ++ "\n\n")
  print "-- TESTING v2"
  putStr(show (filterOddC ([]::[Int])) ++ "\n")
  putStr(show (filterOddC [1, 2, 3, 4, 5]) ++ "\n")
  putStr(show (filterOddC ["a", "b", "c", "d", "e"]) ++ "\n\n")