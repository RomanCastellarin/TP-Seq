-- Implementación del TAD secuencia como Listas 

module ListSeq where

import Seq
import Par
    
instance Seq [] where
    emptyS          = emptyL
    singletonS      = singletonL
    lengthS         = lengthL
    nthS            = nthL
    tabulateS       = tabulateL
    mapS            = mapL
    filterS         = filterL
    appendS         = appendL
    takeS           = takeL
    dropS           = dropL
    showtS          = showtL
    showlS          = showlL
    joinS           = joinL
    reduceS         = reduceL
    scanS           = scanL
    fromList        = id

emptyL = []

singletonL x = [x]

lengthL = length

nthL s i = s !! i

-- How about this?
-- tabulateL f n = mapL f [0..n-1]
tabulateL f n   | n <= 0 = []
                | otherwise = tabulateLAux f 0 n
                    where tabulateLAux f i n    | i == n    = []
                                                | otherwise = let (l,r) = (f i) ||| (tabulateLAux f (i+1) n) in l:r

mapL f []     = []
mapL f [x]    = [f x]
mapL f (x:xs) = let (x',xs') = f x ||| (mapL f xs) in x':xs'

filterL f []     = []
filterL f [x]    = if f x then [x] else []
filterL f (x:xs) = let (x',xs') = filterL f [x] ||| (filterL f xs) in x'++xs'

-- appendL p q = p ++ q    (Depends on the size of the first argument)
appendL = (++)

takeL s n = take n s
dropL s n = drop n s
 
showtL []  = EMPTY
showtL [x] = ELT x
showtL s   = let mid = div (lengthL s) 2
                 (l,r) = (takeL s mid) ||| (dropL s mid)
                in NODE l r
 
showlL []     = NIL
showlL (x:xs) = CONS x xs
 
joinL = concat


contractL f []       = []
contractL f [x]      = [x]
contractL f (x:y:xs) = let (x',xs') = f x y ||| contractL f xs
                        in x':xs'


reduceL f e []         = e
reduceL f e [x]        = x
reduceL f e s@(x:y:xs) = reduceL f e (contractL f s)


expandL f [] _             = []
expandL f _ []             = []
expandL f (x:xs) [y]       = x:[f x y]
expandL f (x:xs) (y:y':ys) = x:(f x y):expandL f xs ys


scanL f e []  = ([],e)
scanL f e [x] = ([x],e)
scanL f e s   = let (l,h) = scanL f e (contractL f s)
                in (expandL f l s, h)


-- scanL (\x->(\y->x++"+"++y)) "E" (fromList ["0", "1", "2", "3", "4"])

{- 
    -- Actually worse
   
tabulateLAux f l r  | (r - l) < 1 = []
                    | otherwise = let mid = div (r + l) 2
                                    (l',r') = tabulateLAux f l mid ||| tabulateLAux f (mid+1) r
                                in l' ++ [f mid] ++ r'
 
tabulateL f n   | n <= 0 = []
                | otherwise = tabulateLAux f 0 n

-}
