{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

import Prelude (($), Int, String, Num, Show, Read, Double, (+), (*), (-), (/), Ord, Eq, Bool(True, False))
import qualified Data.List as List
import Control.Monad
import Data.Monoid

newtype State s w a = State { runState :: s -> (a,s,w) }

class Monad m => MonadState s m | m -> s where 
	get :: m s
	put :: s -> m ()

instance (Monoid v) => Monad (State s v) where  
    return x = State $ \s -> (x,s,mempty)  
    State h >>= f = State $ \s -> let (a, s2, w1) = h s in  let (b, s3, w2) = runState (f a) s2 in (b, s3, mappend w1 w2)

instance (Monoid [String]) => MonadState s (State s [String]) where  
	get = State $ \s -> (s, s, ["get"])
	put s = State $ \_ -> ((), s, ["put"])



type Graph a = a -> [a]
data GGraph a = Graph a [a] deriving (Show,Read)

gEmpty :: GGraph a


