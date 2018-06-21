{- Implementación del TAD secuencia como Listas -}

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

tabulateL f n   | n <= 0 = []
                | otherwise = tabulateL' f 0 n
                    where tabulateL' f i n  | i == n    = []
                                            | otherwise = let (l,r) = (f i) ||| (tabulateL' f (i+1) n) in l:r

mapL f []     = []
mapL f (x:xs) = let (x',xs') = f x ||| (mapL f xs) in x':xs'

filterL f []     = []
filterL f (x:xs) = let (x',xs') = (if f x then [x] else []) ||| (filterL f xs) in x'++xs'

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

contractL :: (a -> a -> a) -> [a] -> [a]
contractL f []       = []
contractL f [x]      = [x]
contractL f (x:y:xs) = let (x',xs') = f x y ||| contractL f xs in x':xs'

reduceL f e []  = e
reduceL f e [x] = f e x
reduceL f e s   = reduceL f e (contractL f s)

expandL :: (a -> a -> a) -> [a] -> [a] -> [a]
expandL f [] _             = []
expandL f _ []             = []
expandL f (x:xs) [y]       = [x]
expandL f (x:xs) (y:y':ys) = x:(f x y):expandL f xs ys

scanL f e []  = ([], e)
scanL f e [x] = ([e], f e x)
scanL f e s   = let (l, res) = scanL f e (contractL f s) in (expandL f l s, res)

