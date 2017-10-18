import Data.List
-- question 6
prefixes :: [t] -> [[t]]
prefixes [] = []
prefixes (x:xs) = addElem x (prefixes xs)
  where
    addElem :: a ->[[a]]->[[a]]
    addElem x [] = [[x]]
    addElem x (y:ys) = (x:y):addElem x ys

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
