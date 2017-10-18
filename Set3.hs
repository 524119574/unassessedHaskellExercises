import Data.List
-- question 9
-- strange, not the result that I expected
perms :: Eq a => [a] -> [[a]]
perms [a] = [[a]]
perms ls = concat [addHead x (perms (ls \\ [x])) | x <- ls]
  where
    addHead :: a -> [[a]] -> [[a]]
    addHead _ [] = []
    addHead x (y:ys) =  (x:y):addHead x ys


-- question 10
