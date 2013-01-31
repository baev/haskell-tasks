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

data Int = Pair Nat Bool deriving (Show,Read)



intZero = Pair Zero True
intOne = Pair (Succ Zero)
intNegOne = Neg (Inc IntZero) 

-- n -> - n
intNeg :: Int -> Int
intNeg (Neg a) = a
intNeg a = Neg a

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp = undefined

intEq :: Int -> Int -> Bool
intEq = undefined

intLe :: Int -> Int -> Bool
intLe = undefined

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
n .+. m = undefined

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. m = undefined

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv = undefined

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp = undefined

ratEq :: Rat -> Rat -> Bool
ratEq = undefined

ratLe :: Rat -> Rat -> Bool
ratLe = undefined

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
n %+ m = undefined

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
n %* m = undefined

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

a7 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
a49 = a7 *. a7
a5 = (Succ (Succ (Succ (Succ (Succ Zero)))))
