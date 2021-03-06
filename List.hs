{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции



-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons _ a) = Succ (length a)

-- Склеить два списка за O(length a)
plusRev :: List a -> List a -> List a
plusRev Nil a = a
plusRev a Nil = a
plusRev (Cons a b) c = plusRev b (Cons a c)

(++) :: List a -> List a -> List a
a ++ b = plusRev (reverse a) b 

-- Список без первого элемента
tail :: List a -> List a
tail (Cons _ a) = a

-- Список без последнего элемента
init :: List a -> List a
init Nil = Nil
init a = reverse (tail (reverse a))

-- Первый элемент
head :: List a -> a
head (Cons a b) = a

-- Последний элемент
last :: List a -> a
last a = head (reverse a)

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero a = Nil
take (Succ n) (Cons a b) = (Cons a Nil) ++ (take n b)  

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero a = a
drop (Succ n) (Cons a b) = drop n b

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a b) = case p a of
	True -> Cons a (filter p b)
	False -> filter p b

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
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
dropWhile = undefined

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p = undefined

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break = undefined

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
l  !! n = undefined

-- Список задом на перёд
reverseTmp :: List a -> List a -> List a
reverseTmp Nil Nil = Nil
reverseTmp Nil a = a
reverseTmp (Cons a b) c = reverseTmp b (Cons a c)

reverse :: List a -> List a
reverse a = reverseTmp a Nil

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = undefined

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat = undefined

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
foldl f z l = undefined

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl = undefined

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
foldr f z l = undefined

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr = undefined

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f l = undefined

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = undefined

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap = undefined

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a b = undefined

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = undefined


a = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
b = Cons 5 (Cons 6 Nil)
c = Cons 7 (Cons 8 (Cons 9 Nil))
