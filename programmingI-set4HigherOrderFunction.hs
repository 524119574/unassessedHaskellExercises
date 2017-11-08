import Data.Char
-- question 2
-- a
depunctuate :: String -> String
depunctuate
  = filter $ flip notElem ".,:"
-- b
makeString :: [Int] -> String
makeString = map chr

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

-- e
rev :: [a] -> [a]
rev xs
  = foldl (flip (:)) [] xs
-- f
dezip :: [(a,b)] -> ([a],[b])
dezip ls
  = (map fst ls, map snd ls)

-- question 3
allSame :: [Int] -> Bool
allSame xs
  = and (zipWith (\ x y -> x==y) xs (tail xs))

-- question 4
factorialList :: [Int]
factorialList
  = scanl (*) 1 [1,2..]

factorialList' :: [Double]
factorialList'
  = scanl (*) 1 [1,2..]

e :: Double
e
  = sum (map ((/) 1) factorialList')

-- let xs = 1 : scanl (+) 1 xs in xs
