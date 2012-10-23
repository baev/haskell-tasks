module ITMOPrelude.Tree where
 
import Prelude ((++), Show, Read, error) 

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Read)

empty = Null


insert :: a -> Tree a -> Tree a
insert a t = Node a t Null

insertLeft :: a -> Tree a -> Tree a
insertLeft a Null = Node a Null Null
insertLeft a (Node value left right) = Node value (insertLeft a left) right

insertRight :: a -> Tree a -> Tree a
insertRight a Null = Node a Null Null
insertRight a (Node value left right) = Node value left (insertRight a right)

rotateLeft :: Tree a -> Tree a
rotateLeft (Node a left Null) = Node a left Null 
rotateLeft (Node a al (Node ar arl arr)) = Node ar (Node a al arl) arr

rotateRight :: Tree a -> Tree a
rotateRight (Node a Null right) = Node a Null right 
rotateRight (Node a (Node al all alr) ar) = Node al all (Node a alr ar)

map :: (a -> b) -> Tree a -> Tree b
map f Null = Null
map f (Node a l r) = Node (f a) (map f l) (map f r)
              
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f z Null = z
foldr f z (Node a l r) = foldr f (f a (foldr f z r)) l


