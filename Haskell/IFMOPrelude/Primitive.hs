{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero _ = LT
natCmp _ Zero = GT
natCmp (Succ a) (Succ b) = natCmp a b

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

natGt :: Nat -> Nat -> Bool
natGt n m = natLt m n

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case n `natLt` m of
	False -> Pair (Succ (fst (natDivMod (n -. m) m))) (snd (natDivMod (n -. m) m))
	True -> Pair Zero n 

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd Zero m = m
gcd n m = gcd (m `natMod` n) n

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным

data Int = Pos Nat | Neg Nat deriving (Show,Read)


intZero = Pos Zero
intOne = Pos (Succ Zero)
intNegOne = Neg Zero 

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = Pos Zero
intNeg (Pos (Succ a)) = Neg a
intNeg (Neg a) = Pos (Succ a)

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Pos a) (Pos b) = natCmp a b
intCmp (Neg a) (Pos b) = LT
intCmp (Pos a) (Neg b) = GT
intCmp (Neg a) (Neg b) = natCmp b a


intEq :: Int -> Int -> Bool
intEq a b = case intCmp a b of
	EQ -> True
	_ -> False

intLt :: Int -> Int -> Bool
intLt a b = case intCmp a b of
	LT -> True
	_ -> False

intGt :: Int -> Int -> Bool
intGt a b = intLt b a

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos a) .+. (Pos b) = Pos (a +. b)
(Neg a) .+. (Neg b) = Neg ((Succ a) +. b)
(Pos a) .+. (Neg b) = case natLt a b of
	True -> Neg (b -. a)
	False -> Pos (a -. b -. natOne)
a .+. b = b .+. a

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.

(Pos a) .*. (Pos b) = Pos (a *. b)
(Neg a) .*. (Neg b) = Pos ((Succ a) *. (Succ b))
(Neg a) .*. (Pos b) = Neg ((Succ a) *. b -. natOne)
a .*. b = b .*. a

infixl 7 .*
(.*) :: Int -> Nat -> Int
x .* d = x .*. (Pos d)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show,Read)

normalizeRat :: Rat -> Rat
normalizeRat (Rat (Pos b) c) = Rat (Pos (natDiv b (gcd b c))) (natDiv c (gcd b c))
normalizeRat (Rat (Neg b) c) = Rat (Neg (natDiv b (gcd b c))) (natDiv c (gcd b c))

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = normalizeRat (Rat (intNeg x) y)

-- У рациональных ещё есть обратные элементы

ratInv :: Rat -> Rat
ratInv (Rat (Pos b) c) = Rat (Pos c) b
ratInv (Rat (Neg b) c) = Rat (Neg c) b 

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a1 b1) (Rat a2 b2) = intCmp (a1 .* b2) (a2 .* b1) 

ratEq :: Rat -> Rat -> Bool
ratEq a b = case ratCmp a b of
	EQ -> True
	_ -> False

ratLt :: Rat -> Rat -> Bool
ratLt a b = case ratCmp a b of
	LT -> True
	_ -> False

ratGt :: Rat -> Rat -> Bool
ratGt a b = ratLt b a

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a1 b1) %+ (Rat a2 b2) = normalizeRat (Rat (a1 .* b2 .+. a2 .* b1) (b1 *. b2))
 
(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a1 b1) %* (Rat a2 b2) = normalizeRat (Rat (a1 .*. a2) (b1 *. b2))

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)


-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b

n1 = Succ Zero
n2 = Succ (Succ Zero)
n3 = Succ (Succ (Succ Zero))
n7 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
n49 = n7 *. n7
n5 = (Succ (Succ (Succ (Succ (Succ Zero)))))
ip5 = Pos n5
ip7 = Pos n7
in5 = Neg (n5 -. natOne)
in7 = Neg (n7 -. natOne)
rp55 = Rat ip5 n5
rp57 = Rat ip5 n7
rn55 = Rat in5 n5
rn57 = Rat in5 n7	
