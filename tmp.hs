

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


-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b






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

-- Тип Знак
data Sign = Neg | Pos deriving (Show,Read)
changeSign :: Sign -> Sign
changeSign Neg = Pos
changeSign Pos = Neg

signEq :: Sign -> Sign -> Bool
signEq Pos Pos = True
signEq Neg Neg = True
signEq _ _ = False

data Int = Int Sign Nat deriving (Show,Read)

Int Neg Zero = Int Pos Zero

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
intCmp (Int Neg a ) (Int Pos b) = LT
intCmp (Int Pos a ) (Int Neg b) = GT
intCmp (Int Neg a ) (Int Neg b) = natCmp b a


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
(Int Pos a) .+. (Int Pos b) = Int Pos (a +. b)
(Int Neg a) .+. (Int Neg b) = Int Neg (a +. b)
(Int Pos a) .+. (Int Neg b) = case natLt a b of
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

ratLt :: Rat -> Rat -> Bool
ratLt = undefined

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

