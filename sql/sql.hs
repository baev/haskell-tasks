module SQL where

import qualified Data.Map as Map
import qualified Data.List as List

bdid = "col_id#"

bdidUsed :: String -> [Map.Map String String] -> Bool
bdidUsed _ [] = False
bdidUsed a (x:xs) = case ((x Map.! bdid) == a) of
	True -> True
	False -> bdidUsed a xs

bdEmpty :: [Map.Map String String]
bdEmpty = [Map.empty]

addCol :: String -> [Map.Map String String] -> [Map.Map String String]
addCol _ [] = []
addCol s (x:xs) = List.insert (Map.insert s "#" x) (addCol s xs)

addLine :: [Map.Map String String] -> [Map.Map String String]
addLine a = List.insert Map.empty a

bdSetLastVal :: String -> String -> [Map.Map String String] -> [Map.Map String String]
bdSetLastVal a b (x:[]) = [(Map.insert a b x)]
bdSetLastVal a b (x:xs) = List.insert x (bdSetLastVal a b xs)

bdCheckP :: String -> [Map.Map String String] -> [Map.Map String String]
bdCheckP s (x:xs) = case (Map.member s x) of 
	True -> (x:xs)
	False -> addCol s (x:xs)

bdInsertAll :: [String] -> [String] -> [Map.Map String String] -> [Map.Map String String]
bdInsertAll a b c = bdInsertTmpAddLine a b (addLine c)

bdInsertTmpAddLine :: [String] -> [String] -> [Map.Map String String] -> [Map.Map String String]
bdInsertTmpAddLine [] [] a = a
bdInsertTmpAddLine (a:as) (b:bs) c = bdInsertTmpAddLine as bs (bdInsertTmp a b c)


bdInsertTmp :: String -> String -> [Map.Map String String] -> [Map.Map String String]
bdInsertTmp a b c = (bdSetLastVal a b (bdCheckP a c))

bdHead :: [Map.Map String String] -> Map.Map String String
bdHead a = List.head a

insert :: String -> [Map.Map String String]
insert " " = []

p33 = bdEmpty
p = List.insert Map.empty p33
p1 = addCol "name" p
p2 = addCol "Last name" p1

k1 = ["k1", "k2", "k3", "k4"]
v1 = ["v1", "v2", "v3", "v4"]

insert = "INSERT INTO phone_book (name, NUMBER) VALUES ('John Doe', '555-1212')"
