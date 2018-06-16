{- Implementación del TAD secuencia como Vectors -}

module ArrSeq where

import qualified Arr as A
import Arr((!))

import Seq

import Par((|||))
    
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
   joinS        = A.flatten
   reduceS      = reduceA
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

 -- ~ NOTE: this is O(log N) and hence reduceA would be O(log² N) span
ilg 1 = 0
ilg n = 1 + ilg (n `div` 2)

-- ~ Pretty implementation

reduceA f e s = case A.length s of
    0 -> e
    _ -> f e (reduceT f s)
    where   reduceT f s = case A.length s of
                1 -> s ! 0 
                _ -> f r1 r2
                where b = 2^(ilg (A.length s - 1))
                      s1 = takeA s b
                      s2 = dropA s b
                      (r1, r2) = (reduceT f s1) ||| (reduceT f s2) 
    
-- ~ Arguably Efficient implementation
    
contractA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contractA f s | n == 1    = s
              | even n    = A.tabulate g  (n `div` 2)
              | otherwise = A.tabulate g' (n `div` 2 + 1)
    where n = A.length s
          g i = f (s ! (2*i)) (s ! (2*i + 1)) -- ugh maybe this can be bettered?
          g' i = if i == (n `div` 2) then s ! (2*i) else g i
          
reduceA' f e s = case A.length s of
    0 -> e
    _ -> f e (reduceByContraction f s)
    where reduceByContraction f s = case A.length s of
            1 -> s ! 0
            _ -> reduceByContraction f (contractA f s)
