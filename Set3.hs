import Data.List
-- question 9
-- strange, not the result that I expected
perms :: Eq a => [a] -> [[a]]
perms [] = []
perms ls = concat [addHead x (perms (ls\\[x])) | x <- ls]

addHead :: a -> [[a]] -> [[a]]
addHead x [] = [[x]]
addHead x (y:ys) =  (x:y):addHead x ys


-- question 10
