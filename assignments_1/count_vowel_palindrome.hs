-- EXERCISE 7
-- utility check if Palindrome
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
-- utility check if Vowel
isVowel :: Char -> Bool
isVowel c = elem c "aeiou"
-- utility get number of Vowels
nVowel :: String -> Int
nVowel [] = 0
nVowel (a:as) = if (isVowel a) then 1 + nVowel as else nVowel as

-- v1: recursion
countVowelPaliR :: [String] -> Int
countVowelPaliR [] = 0
countVowelPaliR (a:as) = if (isPalindrome a) 
                            then nVowel a + countVowelPaliR as 
                            else countVowelPaliR as
-- v2: combinators (concatMap)
countVowelPaliC :: [String] -> Int
countVowelPaliC ss = length $ concatMap (filter (isVowel)) $ filter (isPalindrome) ss

-- tests
main :: IO ()
main = do
  print "-- TESTING v1"
  putStr(show (countVowelPaliR [] ) ++ "\n")
  putStr(show (countVowelPaliR ["apple", "cat"]) ++ "\n")
  putStr(show (countVowelPaliR ["anna", "apple", "civic", "cat"]) ++ "\n\n")
  print"-- TESTING v2"
  putStr(show (countVowelPaliC []) ++ "\n")
  putStr(show (countVowelPaliC ["apple", "cat"]) ++ "\n")
  putStr(show (countVowelPaliC ["anna", "apple", "civic", "cat"]) ++ "\n")

  