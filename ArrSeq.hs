{- Implementación del TAD secuencia como Vectors -}

module ArrSeq where

import qualified Arr as A
import Arr((!))

import Seq

import Par((|||))

(//) = div
    
instance Seq A.Arr where
   emptyS       = emptyA        -- optimal
   singletonS   = singletonA    -- optimal
   lengthS      = lengthA       -- optimal
   nthS         = (!)           -- optimal
   tabulateS    = tabulateA     -- optimal
   mapS         = mapA          -- optimal
   filterS      = filterA       -- optimal
   appendS      = concatA       -- optimal
   takeS        = takeA         -- optimal
   dropS        = dropA         -- optimal
   showtS       = showtA        -- optimal
   showlS       = showlA        -- optimal
   joinS        = flattenA      -- optimal
   reduceS      = reduceA
   scanS        = scanA
   fromList     = fromListA     -- optimal

emptyA = A.fromList []

singletonA x = A.fromList [x]

lengthA = A.length 

tabulateA = A.tabulate

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
          m = n//2

showlA x | n == 0    = NIL
         | otherwise = CONS (x ! 0) (dropA x 1)
    where n = A.length x

flattenA = A.flatten

contractA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contractA f s | n == 1    = s
              | even n    = A.tabulate g  (n//2)
              | otherwise = A.tabulate g' (n//2 + 1)
    where n = A.length s
          g i = f (s ! (2*i)) (s ! (2*i + 1))
          g' i = if i == (n//2) then s ! (2*i) else g i
          
reduceA f e s = case A.length s of
    0 -> e
    _ -> f e (reduceByContraction f s)
    where reduceByContraction f s = case A.length s of
            1 -> s ! 0
            _ -> reduceByContraction f (contractA f s)

scanA f e s = (scan_seq, scan_last)
    where (scan_seq, scan_last) = (scanA' f e s) ||| (reduceA f e s)
          scanA' f e s = case A.length s of
            0 -> emptyA
            1 -> singletonA e
            n -> tabulateA g n
            where s' = scanA' f e (contractA f s)
                  g i | even i    = s' ! (i//2)
                      | otherwise = f (s' ! (i//2)) (s ! (i-1))

fromListA = A.fromList

-- ~ reduceA (\x->(\y->"("++x++"+"++y++")")) "E" (A.fromList ["0", "1", "2", "3", "4", "5", "6"])

-- ~ scanA (\x->(\y->x++"+"++y)) "E" (A.fromList ["0", "1", "2", "3", "4"])
-- ~ (<"E","E+0","E+0+1","E+0+1+2","E+0+1+2+3">,"E+0+1+2+3+4")

-- ~ Testing

-- ~ scanA (\x->(\y->x++"+"++y)) "E" (tabulateA (\x->show x) 10)
