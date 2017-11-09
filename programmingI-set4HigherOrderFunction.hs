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
-- this is the fibnacii series

-- first implementation

-- squash' :: (a-> a -> b) -> [a] -> [b]
-- squash' f xs ys
--   = squash'' f xs (tail ys) 
--   where
--     squash'' :: (a-> a -> b) -> [a] -> [b]
--     squash'' f (x:xs) (y:ys)
--       = f x y: squash'' f xs ys

-- second implementation

-- squash :: (a-> a -> b) -> [a] -> [b]
-- squash f xs ys = zipWith f xs ys

-- question 6
converge :: Eq a => (a -> a -> Bool) -> [a] -> a
converge f xs
  = converge' f xs (tail xs)
  where
    converge' :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> a
    converge' f (x:xs) (y:ys)
      | f x y == True || xs == [] = x
      | otherwise = converge' f xs ys


-- question 10
any :: (a -> Bool) -> [a] -> Bool
any p
  = or . map p
all :: (a -> Bool) -> [a] -> Bool
all p
  = and . map p

  -- question 12
infixl 8 <.>
(<.>) :: (a->b)->(c->d->a) -> (c->d->b)
(<.>) f g
  = h
    -- \x y = f $ g x y
  where
    h x y = f $ g x y
--  or you can use lambda expression

-- quick sort
qsort [] = []
qsort (a:as) = qsort left ++ [a] ++ qsort right
  where (left,right) = (filter (<=a) as, filter (>a) as)

main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])