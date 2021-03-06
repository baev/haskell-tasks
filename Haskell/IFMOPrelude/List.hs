{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive


data List a = Nil |  Cons a (List a) deriving (Show,Read)

length :: List a -> Nat
length Nil = Zero
length (Cons _ a) = Succ (length a)

plusRev :: List a -> List a -> List a
plusRev Nil a = a
plusRev a Nil = a
plusRev (Cons a b) c = plusRev b (Cons a c)

(++) :: List a -> List a -> List a
a ++ b = plusRev (reverse a) b 

tail :: List a -> List a
tail Nil = Nil
tail (Cons _ a) = a

init :: List a -> List a
init Nil = Nil
init a = reverse (tail (reverse a))

head :: List a -> a
head Nil = error "There is no such element"
head (Cons a b) = a

last :: List a -> a
last a = head (reverse a)

take :: Nat -> List a -> List a
take _ Nil = error "List Index Out of range"
take Zero a = Nil
take (Succ n) (Cons a b) = (Cons a Nil) ++ (take n b)  

drop :: Nat -> List a -> List a
drop _ Nil = error "Out of range"
drop Zero a = a
drop (Succ n) (Cons a b) = drop n b

filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a b) = case p a of
	True -> Cons a (filter p b)
	False -> filter p b

gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a b) = case p a of
	Nothing -> gfilter p b
	Just c -> Cons c (gfilter p b)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a b) = case p a of
	False -> Nil
	True -> Cons a (takeWhile p b)

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a b) = case p a of
	True -> dropWhile p b
	False -> Cons a b

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair (Nil) (Nil)
span p (Cons a b) = case p a of
	True -> Pair (Cons a (first)) second where
		Pair first second = span p b 
	False -> Pair Nil (Cons a b)

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a b) !! Zero = a
(Cons a b) !! n = b !! (n -. natOne) 

-- Список задом на перёд
reverse' :: List a -> List a -> List a
reverse' Nil a = a
reverse' (Cons a b) c = reverse' b (Cons a c)

reverse :: List a -> List a
reverse a = reverse' a Nil

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = t ++ (map (Cons x) t) where t = subsequences xs

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a b) = foldl f (f z a) b

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a b) = Cons fza (scanl f fza b) where fza = f z a

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a b) = f a (foldr f z b)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons a b) = Cons (f a (head list)) list where list = scanr f z b

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a Nil) = a
concat (Cons a (Cons b c)) = concat (Cons (a ++ b) c)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a b) = f a ++ concatMap f b

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip = zipWith Pair

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons a b) (Cons c d) = Cons (f a c) (zipWith f b d)


a = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
b = Cons 5 (Cons 6 Nil)
c = Cons 7 (Cons 8 (Cons 9 Nil))
