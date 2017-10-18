import Data.List
-- 
-- First Part Basics
-- 

precedes :: String -> String -> Bool
precedes ns ms = ns <= ms

-- 3
pos :: Int -> [Int] -> Int
pos int ints = head [x | x <- [0..length ints], ints !! x == int]
-- might need to handle exception, I can use element here as well 

-- 4
twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (n:ns) = elem n ns || twoSame ns
-- time complexity is n^2

-- 5
rev :: [a] -> [a]
rev [] = []
rev (n:ns) = rev ns ++ [n] 

-- 6
isSubstring :: String -> String -> Bool
isSubstring _ [] = False
isSubstring [] _ = True
isSubstring (m:ms) (n:ns)
  | m == n = True && isSubstring ms ns
  | otherwise = isSubstring (m:ms) ns


ints :: Int -> [Int]
ints n
  | n == 0 = []
  | n > 0 = n:ints(n-1)

ints n
  =  ints' 1
  where
    ints' k
      | k > n = []
      | otherwise = k : ints' (k+1)

-- 
-- Second Part List Comprehension
-- 

-- question 1
findAll :: Int -> [(Int, Int)] -> [Int]
findAll x t  = [y' | (x',y') <- t, x' == x]

-- question 2

-- question 3
remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove x tables = [(x',y') | (x',y') <- tables, x' /= x]

remove' :: Eq a => a -> [(a,b)] -> [(a,b)]
remove' x tables = error "further work is needed to complete this function"


  
-- question 6
-- not a very good implementation
prefixes :: [t] -> [[t]]
prefixes ls = reverse (prefixes' ls)
  where
    addElem :: a ->[[a]]->[[a]]
    addElem x [] = [[x]]
    addElem x (y:ys) = (x:y):addElem x ys
    prefixes' :: [t] -> [[t]]
    prefixes' [] = []
    prefixes' (x:xs) = addElem x (prefixes' xs)


-- question 8
subString :: String -> [String]
subString = error "subString is still undefined yet"

-- question 9
-- strange, not the result that I expected
perms :: Eq a => [a] -> [[a]]
perms [a] = [[a]]
perms ls = concat [addHead x (perms (ls \\ [x])) | x <- ls]


-- question 10


addHead :: a -> [[a]] -> [[a]]
addHead _ [] = []
-- uncomment below line if you want to get all permutation from length 1 to
-- n
-- addHead x [] = [[x]]
addHead x (y:ys) =  (x:y):addHead x ys
