module Game where 

--import Data.Set as Set
import qualified Data.Set as Set

--import Data.List as ListLL
import qualified Data.List as List


data XY = Pair (Int, Int) deriving (Ord,Show,Read)

instance Eq XY where
	Pair (a, b) == Pair (c, d) = b == d && a == c

data MBody = Body XY Bool Int deriving (Ord,Show,Read)

instance Eq MBody where
	Body a1 b1 c1 == Body a2 b2 c3 = a1 == a2


xy1 = Pair(0, 0)
xy2 = Pair(0, 1)
xy3 = Pair(0, 2)
xy4 = Pair(1, 2)
xy5 = Pair(2, 1)
s = Set.empty

--l = [Pair(2, 5), Pair(2, 6), Pair(3, 5), Pair(3, 6), Pair(12, 4), Pair(12, 5), Pair(12, 6),
--	Pair(13, 3), Pair(13, 7), Pair(14, 2), Pair(15, 2) , Pair(14, 8), 
--	Pair(15, 8), Pair(16, 5), Pair(17, 3), Pair(17, 7), Pair(18, 4), Pair(18, 5),
--	Pair(18, 6), Pair(19, 5), Pair(22, 6), Pair(22, 7), Pair(22, 8),
--	Pair(23, 6), Pair(23, 7), Pair(23, 8), Pair(24, 5), Pair(24, 9), Pair(26, 5), 
--	Pair(26, 9), Pair(26, 4), Pair(26, 10),Pair(36, 8), Pair(36, 7),Pair(37, 8), Pair(37, 7)]

l = [xy1, xy2, xy3, xy4, xy5]

l1 = List.insert xy1 l

givICell :: XY -> MBody
givICell a = Body a True 0

givICellList :: [XY] -> [MBody]
givICellList [] = []
givICellList (x:xs) = List.insert (givICell x) (givICellList xs)

f :: [MBody] -> [MBody]
f [] = []
f (x:xs) = List.insert x (f xs)

--6 7 8
--4 * 5 
--1 2 3

xx = [0, 0, 1, -1]
yy = [1, -1, 0, 0]

getNB :: XY -> [XY]
getNB a = getNBtmp a 3

getNBtmp :: XY -> Int -> [XY]
getNBtmp a (-1) = []
getNBtmp (Pair (x, y)) n = List.insert 
	(Pair ((x + (xx !! n)), (y + (yy !! n))))
	(getNBtmp (Pair (x, y)) (n - 1))


p = givICellList l
cc = getNB xy1


--sss = take 10 (c game p)

--splitByX :: [XY] -> [[XY]]
getCh :: Int -> Char -> [Char]
getCh 0 _ = []
getCh n c = case (n > 0) of
	True -> List.insert c (getCh (n - 1) c) 
	False -> []

toXY :: [MBody] -> [XY]
toXY [] = []
toXY ((Body xy a b):xs) = List.insert xy (toXY xs)

lM :: [XY] -> Int
lM ((Pair (x, y)):[]) = x
lM ((Pair (x, y)):xs) = min x (lM xs)

rM :: [XY] -> Int
rM ((Pair (x, y)):[]) = x
rM ((Pair (x, y)):xs) = max x (rM xs)

uM :: [XY] -> Int
uM ((Pair (x, y)):[]) = y
uM ((Pair (x, y)):xs) = max y (uM xs)

dM :: [XY] -> Int
dM ((Pair (x, y)):[]) = y
dM ((Pair (x, y)):xs) = min y (dM xs)

gout :: [XY] -> [Char]
gout a = fout (List.sort a) (Pair((lM a) - 1, (tmp))) (tmp) where tmp = dM a

fout :: [XY] -> XY -> Int -> [Char]
fout [] _ _ = []
fout ((Pair (x1, y1)):xs) (Pair (x2, y2)) miny = case (x1 == x2) of
	True -> (getCh (y1 - y2 - 1) ' ') ++ ['*'] ++ (fout xs (Pair (x1, y1)) miny) 
	False -> (getCh (x1 - x2) '\n') ++ (getCh (y1 - miny) ' ') ++ ['*'] ++ (fout xs (Pair (x1, y1)) miny) 


game2 :: [MBody] -> IO ()
game2 [] = putStrLn "###########" 
game2 a = do
	putStrLn (gout (toXY a))
	putStrLn "###########" 
	game2 (game a)

game :: [MBody] -> [MBody]
game a = case ((List.length a) > 0) of
	True -> do 
			--tmp
			--putStrLn "aaa"
			tmp
			where tmp = (aul (start a))
	False -> []
		
	

aul :: [MBody] -> [MBody]
aul [] = []
aul ((Body a b c):xs) = case b of
	True -> (List.insert (Body a b c) (aul xs))
	False -> (aul xs)

start :: [MBody] -> [MBody]
start a = update (getAllNB a) a

getAllNB :: [MBody] -> [XY]
getAllNB [] = []
getAllNB ((Body a b c):xs) = case b of 
	True -> ((getNB a) ++ getAllNB xs)
	False -> getAllNB xs

update :: [XY] -> [MBody] -> [MBody]
update a b = kill (updateL a (clearStats b))

updateL :: [XY] -> [MBody] -> [MBody]
updateL (x:[]) a = (updateNB x a)
updateL (x:xs) a = updateL xs (updateNB x a)

kill :: [MBody] -> [MBody]
kill [] = []
kill ((Body a b c):xs) = case ((c < 2) || (c > 3)) of
	True -> case (c > 0) of
		True -> List.insert (Body a False c) (kill xs)
		False -> (kill xs)
	False -> List.insert (Body a True c) (kill xs)

--апдатит клеточку xyb 
updateNB :: XY -> [MBody] -> [MBody]
updateNB a b = updateNBtmp a b False

clearStats :: [MBody] -> [MBody]
clearStats [] = []
clearStats ((Body a b c):xs) = List.insert (Body a b 0) (clearStats xs)

updateNBtmp :: XY -> [MBody] -> Bool -> [MBody]
updateNBtmp a [] False = [(Body a True 1)]
updateNBtmp a [] True = []

updateNBtmp a ((Body b c d):xs) False = case ((b == a) && c) of
	True -> List.insert (Body b c (d + 1)) (updateNBtmp a xs True)
	False -> List.insert (Body b c d) (updateNBtmp a xs False)

updateNBtmp a ((Body b c d):xs) True = 
	List.insert (Body b c d) (updateNBtmp a xs True)

--inc :: Body -> MapBody



