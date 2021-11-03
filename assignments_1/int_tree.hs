-- EXERCISE 9
import Data.Tree

-- data (using 'Branch' because of conflict with with Data.Tree.Node)
data IntTree = Leaf Int | Branch (Int, IntTree, IntTree)
-- utility for making a tree printable with drawTree
toDataTree (Leaf a) = Node a []
toDataTree (Branch (b, cs, ds)) = Node b [toDataTree cs, toDataTree ds]

-- tmap : recursion
tmap :: (Int -> Int) -> IntTree -> IntTree
tmap f (Leaf x) = Leaf (f x)
tmap f (Branch (x, xl, xr)) = Branch(f x, tmap f xl, tmap f xr)
-- succTree
succTree :: IntTree -> IntTree
succTree t = tmap (+1) t
-- sumuScc : recursion
sumSucc :: IntTree -> Int
sumSucc t = sumTree (succTree t) where 
  sumTree t1 = case t1 of
                    (Leaf x) -> x
                    (Branch(x, xl, xr)) -> x + sumTree xl + sumTree xr

-- tests
main :: IO ()
main = do
  let t1 = Leaf 1
  let t2 = Branch(1, Leaf 2, Leaf 3)
  let t3 = Branch(1, Branch(2, Leaf 3, Leaf 4), Branch(5, Leaf 6, Leaf 7))

  print "-- TESTING succTree"
  print "-- one element tree"
  putStr(drawTree $ fmap show $ toDataTree $ succTree t1)
  print "-- root and two leaves"
  putStr(drawTree $ fmap show $ toDataTree $ succTree t2)
  print "-- with subtrees"
  putStr(drawTree $ fmap show $ toDataTree $ succTree t3)

  print "-- TESTING sumSucc"
  print "-- one element tree"
  putStr(show (sumSucc t1) ++ "\n")
  print "-- root and two leaves"
  putStr(show (sumSucc t2) ++ "\n")
  print "-- with subtrees"
  putStr(show (sumSucc t3) ++ "\n")
  