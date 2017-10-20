import Data.Char
-- question 2
-- a
depunctuate :: String -> String
depunctuate
  = filter $ flip notElem ".,:"
-- b
-- makeString :: [Int] -> String
-- makeString = concatMap chr

-- c
enpower :: [Int] -> Int
enpower (l:ls)
  = foldl (flip (^)) l ls

enpower' :: [Int] -> Int
enpower'
  = foldl1 $ flip (^)
  
-- d
revAll :: [[a]] -> [a]
revAll
  = concatMap reverse
-- another method
revAll' :: [[a]] -> [a]
revAll' ls
  = concat [reverse l | l <- ls]
-- f
dezip :: [(a,b)] -> ([a],[b])
dezip ls
  = (map fst ls, map snd ls)