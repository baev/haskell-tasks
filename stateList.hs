
import Data.Monoid

newtype Logger w a = Logger { runWriter :: (a, w) } 

instance (Monoid w) => Monad (Logger w) where  
    	return x = Logger (x, mempty)  
    	(Logger (x,v)) >>= f = let (Logger (y, v')) = f x in Logger (y, v `mappend` v') 
	 
tell s = Logger ((),s)

logNumber :: Int -> Logger [String] Int  
logNumber x = Logger (x, ["got " ++ show x])  
  
multWithLog :: Int -> Int -> Logger [String] Int  
multWithLog x y = do  
    	a <- logNumber x  
    	b <- logNumber y  
	tell ["multiply"]
    	return (a*b)  

sumWithLog :: Int -> Int -> Logger [String] Int  
sumWithLog x y = do  
    	a <- logNumber x  
    	b <- logNumber y  
	tell ["summarize"]
    	return (a+b)  
