-- Question 1
data Shape = Tri (Float, Float, Float)
           | Sq Float | Cir Float | Poly [(Float, Float)]
data Tree a = Empty | Node (Tree a) a (Tree a)
area :: Shape -> Float
area (Tri (a,b,c))
  = sqrt $ s*(s-a)*(s-b)*(s-c)
    where
     s = (a+b+c)/2

area (Sq l)
  = l^2

area (Cir r)
  = pi*r^2
-- Question 2
area (Poly (p1:p2:p3:ps))
  | ps == [] = area (Tri (a,b,c))
  | otherwise = area (Tri (a,b,c)) + area (Poly (p2:p3:ps))
    where
      a = getLength p1 p2
      b = getLength p2 p3
      c = getLength p1 p3
      getLength :: (Float, Float) -> (Float, Float) -> Float
      getLength (x1,y1) (x2, y2)
        = sqrt $ (x1-x2)^(2) + (y1-y2)^(2)
-- Question 3
type Date = (Int, Int, Int)
type Age = Int
age :: Date -> Date -> Age
age cd bd
  = currentYear - birthYear
    where
      (currentYear, _, _) = cd
      (birthYear, _, _) = bd

-- Question 4
flatten :: Tree a -> [a]
flatten Empty
  = []
flatten (Node t1 x t2)
  = flatten t1 ++ (x:flatten t2)

-- remember this function!!!
flatten' :: Tree a -> [a]
flatten' Empty
  = []
flatten' t
  = flatten'' t []
    where
      flatten'' :: Tree a -> [a] -> [a]
      flatten'' Empty acc
        = acc
      flatten'' (Node t1 x t2) acc
        = flatten'' t1 (x:flatten'' t2 acc)

-- question 5
data Tree' = Leaf | Node' Tree' Tree'
  deriving (Eq, Show)

makeTrees :: Int -> [Tree']
makeTrees 0
  = [Leaf]
makeTrees 1
  = [Node' Leaf Leaf]
makeTrees 2
  = [Node' (Node' Leaf Leaf) Leaf, Node' Leaf (Node' Leaf Leaf)]

makeTrees n
  = concat [[(Node' Leaf tree), (Node' tree Leaf)] | tree <- (makeTrees (n-1))]

addNode :: Tree' -> Tree'
addNode Leaf
  = Node' Leaf Leaf
addNode (Node' t1 t2)
  = Node' (addNode t1) t2


-- addAllNode :: Tree' -> [Tree']
-- addAllNode
-- addAllNode (Node t1 t2)
--   = addNode t1 : [addNode t2]
-- addAllNode (Node' t1 t2)
--   = addAllNode t1 ++ addAllNode t2


-- question 6
data MyTree a = MyLeaf a | MyNode (MyTree a) (MyTree a)
  deriving (Eq, Show)

build :: [a] -> MyTree a
build [x]
  = MyLeaf x
build ls
  = MyNode (build l) (build r)
    where
      (l,r) = splitHalf ls
      splitHalf :: [a] -> ([a],[a])
      splitHalf = splitAt ((length ls) `div` 2)

ends :: MyTree a -> [a]
ends (MyLeaf x)
  = [x]
ends (MyNode t1 t2)
  = ends t1 ++ ends t2
-- is it possible to make it tail recursive?

-- question c
--  we know that `ends (swap (build xs)) == reverse xs`
swap :: MyTree a -> MyTree a
swap (MyLeaf a)
  = MyLeaf a

swap (MyNode t1 t2)
  = MyNode (swap t2) (swap t1)

data SuperTree a b = SuperLeaf b | SuperNode a (SuperTree a b) (SuperTree a b)
                    | SuperEmpty

mapT :: (b -> d) -> (a -> c) -> SuperTree a b-> SuperTree c d
mapT _ _ SuperEmpty
  = SuperEmpty
mapT f g (SuperLeaf b)
  = SuperLeaf (f b)
mapT f g (SuperNode a t1 t2)
  = SuperNode (g a) (mapT f g t1) (mapT f g t2)

foldT :: (a -> d -> d -> c) -> (b -> d) -> c -> SuperTree a b -> c
foldT _ _ b SuperEmpty
  = SuperEmpty
