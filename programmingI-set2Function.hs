-- 1
addDigit :: Int -> Int -> Int
addDigit n m = 10*n + m

-- 2
cToF :: Float -> Float
-- not here that you cannot use `/` in`Int` 
cToF c = c * 9.0 / 5.0 + 32

-- 3
type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

-- 4
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea p1 p2 p3 = let a = distance p1 p2
                            b = distance p1 p3
                            c = distance p2 p3
                        in (1/4)*sqrt ((a+b+c)*(b+c-a)*(a+c-b)*(a+b-c))
-- 5
isPrime :: Int -> Bool
isPrime n = length [x | x <- [1..n], n `mod` x == 0] <= 2

-- 6
fact :: Int -> Int
fact 1 = 1
fact n = n*fact(n-1)

-- 7
perm :: Int -> Int -> Int
perm n 1 = n 
perm n r = n * perm (n-1) (r-1)

-- 8
choose :: Int -> Int -> Int
choose n r
  | n == r = 1
choose n r = (n*choose (n-1) r) `div` (n-r)

-- 9
remainder :: Int -> Int -> Int
remainder n m
  | n >= m = remainder (n-m) m
  | otherwise = n

-- 10
quotient :: Int -> Int -> Int
quotient n m
  | n >=m = 1 + quotient (n-m) m
  | otherwise = 0

-- 11
decToBin :: Int -> Int
decToBin n
  | n < 2 = n
  | n >= 2 = (decToBin (n `div` 2)) *10 + n `mod` 2

-- 12
-- a
add :: Int -> Int -> Int
add n 0 = n
add n m = add (succ n) (pred m)

-- b
larger :: Int -> Int -> Int
larger n m
  | n == 0 = m
  | m == 0 = n
  | otherwise = larger (n-1) (m-1) + 1

-- 13

-- 15
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n
  | n > 1 = fib (n-1) + fib (n-2)

-- draft
isADigit :: Char -> Bool
isADigit c = c >= '0' && c <= '9'