{- Implementación del TAD secuencia como Vectors -}

module ArrSeq where

import qualified Arr as A
import Arr((!))

import Seq
    
instance Seq A.Arr where
   emptyS       = emptyA
   singletonS   = singletonA
   lengthS      = A.length 
   nthS         = (!) 
   tabulateS    = A.tabulate
   mapS         = mapA 
   filterS      = filterA 
   appendS      = concatA
   takeS        = takeA
   dropS        = dropA
   showtS       = showtA
   showlS       = showlA
   -- ~ joinS      :: s (s a) -> s a
   -- ~ reduceS    :: (a -> a -> a) -> a -> s a -> a
   -- ~ scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
   fromList     = A.fromList

-- ~ SERIOUS IMPLEMENTATION

emptyA = A.fromList []

singletonA x = A.fromList [x] 

concatA x y = A.flatten (A.fromList [x, y])

mapA f x = A.tabulate g (A.length x)
    where g n = f (x ! n)
    
filterA p x = A.flatten (mapA g x)
    where g y = if p y then singletonA y else emptyA
    
takeA x n = A.subArray 0 n x

dropA x n = A.subArray n (A.length x - n) x

showtA x | n == 0    = EMPTY 
         | n == 1    = ELT (x ! 0)
         | otherwise = NODE (takeA x m) (dropA x m)
    where n = A.length x
          m = n `div` 2

showlA x | n == 0    = NIL
         | otherwise = CONS (x ! 0) (dropA x 1)
    where n = A.length x
