module ITMOPrelude.Primitive where
import Prelude (Show,Read)

undefined = undefined

data Unit = Unit deriving (Show,Read)
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)
data Either a b = Left a | Right b deriving (Show,Read)
data Maybe a = Nothing | Just a deriving (Show,Read)
data Bool = False | True deriving (Show,Read)


data Tri = LE | EQ | GT deriving (Show,Read)
data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero
natOne = Succ Zero


natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero _ = LE
natCmp _ Zero = GT
natCmp (Succ a) (Succ b) = natCmp a b


natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq Zero (Succ _) = False
natEq (Succ _) Zero = False
natEq (Succ n) (Succ m) = natEq n m


natLe :: Nat -> Nat -> Bool
natLe Zero Zero = False
natLe Zero (Succ _) = True
natLe (Succ _) Zero = False
natLe (Succ n) (Succ m) = natLe n m

natGt :: Nat -> Nat -> Bool
natGt n m = natLe m n

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case n `natLe` m of
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

data Sign = Neg | Pos deriving (Show,Read)
changeSign :: Sign -> Sign
changeSign Neg = Pos
changeSign Pos = Neg

signEq :: Sign -> Sign -> Bool
signEq Pos Pos = True
signEq Neg Neg = True
signEq _ _ = False

data Int = Int Sign Nat deriving (Show,Read)

intZero = Int Pos Zero
intOne = Int Pos (Succ Zero)
intNegOne = Int Neg (Succ Zero) 

-- n -> - n
intNeg :: Int -> Int
intNeg (Int Pos Zero) = Int Pos Zero
intNeg (Int a b) = Int (changeSign a) b

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Int Pos a ) (Int Pos b) = natCmp a b
intCmp (Int Neg a ) (Int Pos b) = LE
intCmp (Int Pos a ) (Int Neg b) = GT
intCmp (Int Neg a ) (Int Neg b) = natCmp b a

intEq :: Int -> Int -> Bool
intEq a b = case intCmp a b of
	EQ -> True
	_ -> False

intLe :: Int -> Int -> Bool
intLe a b = case intCmp a b of
	LE -> True
	_ -> False

intGt :: Int -> Int -> Bool
intGt a b = intLe b a

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Int Pos a) .+. (Int Pos b) = Int Pos (a +. b)
(Int Neg a) .+. (Int Neg b) = Int Neg (a +. b)
(Int Pos a) .+. (Int Neg b) = case natLe a b of
	True -> Int Neg (b -. a)
	False -> Int Pos (a -. b)
a .+. b = b .+. a

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int

(Int a b) .*. (Int c d) = case natEq (b *. d) Zero of
	True -> intZero
	False -> case signEq a c of
		True -> Int Pos (b *. d)
		False -> Int Neg (b *. d)

infixl 7 .*
(.*) :: Int -> Nat -> Int
x .* d = x .*. (Int Pos d)
-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show,Read)

normalizeRat :: Rat -> Rat
normalizeRat (Rat (Int a b) c) = Rat (Int a (natDiv b (gcd b c))) (natDiv c (gcd b c))

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = normalizeRat (Rat (intNeg x) y)

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Int a b) c) = Rat (Int a c) b 

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a1 b1) (Rat a2 b2) = intCmp (a1 .* b2) (a2 .* b1) 

ratEq :: Rat -> Rat -> Bool
ratEq a b = case ratCmp a b of
	EQ -> True
	_ -> False

ratLe :: Rat -> Rat -> Bool
ratLe a b = case ratCmp a b of
	LE -> True
	_ -> False

ratGt :: Rat -> Rat -> Bool
ratGt a b = ratLe b a

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

n7 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
n49 = n7 *. n7
n5 = (Succ (Succ (Succ (Succ (Succ Zero)))))
ip5 = Int Pos n5
ip7 = Int Pos n7
in5 = Int Neg n5
in7 = Int Neg n7
rp55 = Rat ip5 n5
rp57 = Rat ip5 n7
rn55 = Rat in5 n5
rn57 = Rat in5 n7
