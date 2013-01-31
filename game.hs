module Main where

import Array
import List
--import Char

data Field = Field (Array (Int,Int) Bool)

instance Show Field where
  show (Field a) = concat [ map disp [ a!(x,y) | x <- [minx..maxx] ] ++ "\n"
                            | y <- [miny..maxy] 
                          ]
                   where disp True  = '*'
                         disp False = ' '
                         ((minx,miny),(maxx,maxy)) = bounds a

-- make game field from set of strings. 
-- space character means no cell, any other character means cell
makeField :: [String] -> Field
makeField rows = Field $ accumArray (||) False ((0,0),(maxx,maxy)) cells
    where maxx = maximum $ map length rows
          maxy = length rows
          cells = [((x,y),True) | (y,row) <- zip [0..] rows,
                                  (x,cell) <- zip [0..] row,
                                  cell /= ' ' ]

evolve :: Field -> Field
evolve (Field src) = Field $ array (bounds src) [ (pos,calc pos) | pos <- range (bounds src) ]
  where calc pos | neighbors pos == 2 = src!pos
                 | neighbors pos == 3 = True
                 | otherwise          = False
        neighbors pos = sum [ if src!p then 1 else 0 | p <- around pos ]
        around (x,y) = map (\(x,y) -> (wrap minx maxx x, wrap miny maxy y))
                       [ (x-1,y-1),(x,y-1),(x+1,y-1),
                         (x-1,y),          (x+1,y), 
                         (x-1,y+1),(x,y+1),(x+1,y+1) ]
        wrap min max x = (min + (x-min) `mod` (max-min+1))
        ((minx,miny),(maxx,maxy)) = bounds src

-- read lines until CR pressed twice
getLines :: IO [String]
getLines = do x <- getLine
              if x == ""
                 then return []
                 else do xs <- getLines
                         return (x:xs)

play field = do print field
                cmd <- getChar
                if (toLower cmd)=='q'
                   then return ()
                   else play (evolve field)

main = do
  getLines >>= (play.makeField)
