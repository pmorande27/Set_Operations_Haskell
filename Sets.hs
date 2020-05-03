module Sets (Set , equal, insertset , unionset , diff, makeSet, Set2, equal2, element2, makeSet2, union2, insertSet2) where 
import Data.List
--Set as a List--
data Set a = Set [a] deriving (Eq, Ord,Show)
--equal--
equal ::Ord a =>Eq a => Set a -> Set a -> Bool
equal (Set a) (Set b) =sort (nub a) ==sort (nub b)

--insert--
insertset :: Set a -> a -> Set a 
insertset (Set a) b = Set (a ++ [b])
--Union--
unionset :: Set a -> Set a -> Set a
unionset (Set a) (Set b) = Set (a ++ b)
--Difference-- -->Elements of the first that are not in the second
diff :: Eq a => Set a -> Set a -> Set a
diff (Set x) (Set [] ) = Set x
diff (Set []) (Set ys) = Set []
diff (Set (x:xs)) (Set ys) 
    |elem x ys = diff (Set xs) (Set ys)
    |otherwise = unionset (Set [x])  (diff (Set xs) (Set ys))
--MakeSet--
makeSet :: [a] -> Set a
makeSet a = Set a

--Set as ordered Lists--
data Set2 a = Set2 [a] deriving (Eq,Show)
--Equal--
equal2 :: Eq a =>Set2 a -> Set2 a -> Bool
equal2 (Set2 a) (Set2 b) = a == b
--Element--
element2 :: Eq a => Ord a => a -> Set2 a -> Bool 
element2 x (Set2 a)
    |makesureSet (Set2 a) = checkelement x (Set2 a)
    |otherwise = checkelement x (makeSet2 a)
checkelement ::Eq a => Ord a => a -> Set2 a -> Bool 
checkelement x (Set2 []) = False
checkelement x (Set2 (y:xs))
    | x > y = False
    | x == y = True
    | x <y = checkelement x (Set2 xs)
--MakeSet--
makeSet2 ::Ord a => [a] -> Set2 a 
makeSet2 x = Set2 (sort (nub x))
--Union 
union2 :: Ord a => Set2 a -> Set2 a -> Set2 a
union2 (Set2 a) (Set2 b) 
    |makesureSet (Set2 a) && makesureSet (Set2 b) =  Set2 (((a ++b )))
    |otherwise = union2 (makeSet2 a) (makeSet2 b)
--Insert--
insertSet2 ::Eq a => Ord a => a -> Set2 a -> Set2 a
insertSet2 x (Set2 b) 
    |makesureSet  (Set2 b) = insert2 x (Set2 b) 
    |otherwise = insert2 x (makeSet2 b)
insert2 :: Eq a => Ord a => a -> Set2 a -> Set2 a
insert2 x (Set2 []) = (Set2 [x])
insert2 x (Set2 (y:xs))
    |x == y = Set2 (y:xs)
    |x >y = union2 (Set2 [y]) (insert2 x (Set2 xs))
    |x< y = Set2 ((x:y: xs)) c
--Check if it is an ordered list fo
makesureSet :: Ord a =>Set2 a -> Bool 
makesureSet (Set2 x) = equal2 (Set2 x) (makeSet2 x)

--Sets as ordered tree--
--Invariant--
invariant :: Ord a => Set3 a -> Bool
invariant Nil = True
invariant (Node l x r) =invariant l && invariant r &&and [ y < x | y <- list l ] && and [ y > x | y <- list r ]
data Set3 a = Nil | Node (Set3 a) a (Set3 a) deriving (Eq, Show)
--union--Pendiente de arreglar
{-union3 :: Eq a => Ord a =>Set3 a -> Set3 a -> Set3 a 
union3 (Nil) a = a 
union3 a (Nil) = a 
union3 (Node a b c) (Node d e f) 
    | b > e = Node d e ( union3 (Node a b c) f)
    | b < e = Node (union3 (Node a b c) c ) e f
    | b == e = Node (union3 a d) e (union3 c f)-}
--MakeSet--
makeSet3 :: Ord a => [a] -> Set3 a -> Set3 a

makeSet3 [] y = y
makeSet3 (x:xs) y =  (makeSet3 xs (insertSet3 x y)) 
--Insertion--
insertSet3 ::Ord a => Eq a=> a -> Set3 a -> Set3 a 
insertSet3 a Nil = Node (Nil) a (Nil) 
insertSet3 a (Node d b c) 
    | a == b = Node d b c 
    | a > b = Node d b (insertSet3 a c)
    | otherwise = Node (insertSet3 a d) b c
--make list--
list :: Set3 a -> [a]
list Nil = []
list (Node l x r) = list l ++ [x] ++ list r
--makeordered list--
ordlist ::Ord a=> [a] -> [a]
ordlist xs = list (makeSet3 xs Nil)


