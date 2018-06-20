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

-- How about this?
-- tabulateL f n = mapL f [0..n-1]
tabulateL f n   | n <= 0 = []
                | otherwise = tabulateL' f 0 n
                    where tabulateL' f i n  | i == n    = []
                                            | otherwise = let (l,r) = (f i) ||| (tabulateL' f (i+1) n) in l:r

mapL f []     = []
mapL f (x:xs) = let (x',xs') = f x ||| (mapL f xs) in x':xs'

filterL f []     = []
filterL f (x:xs) = let (x',xs') = (if f x then [x] else []) ||| (filterL f xs) in x'++xs'


appendL = (++) -- Depends only on the size of the first argument

takeL s n = take n s
dropL s n = drop n s

-- Doesn't actually optimize anything, still O(n)
showtL []  = EMPTY
showtL [x] = ELT x
showtL s   = let mid = div (lengthL s) 2
                 (l,r) = (takeL s mid) ||| (dropL s mid)
                in NODE l r
            -- Should we use this simpler version?
            -- let mid = div (lengthL s) 2 in NODE (takeL s mid) (dropL s mid)
 
showlL []     = NIL
showlL (x:xs) = CONS x xs
 
joinL = concat

reduceL f e [] = e
reduceL f e s  = f e (reduceL' f e s)
    where   reduceL' f e []  = e
            reduceL' f e [x] = x
            reduceL' f e s   = reduceL' f e (contractL f s)

contractL f []       = []
contractL f [x]      = [x]
contractL f (x:y:xs) = let (x',xs') = f x y ||| contractL f xs in x':xs'

expandL f [] _             = []
expandL f _ []             = []
expandL f (x:xs) [y]       = [x]
expandL f (x:xs) (y:y':ys) = x:(f x y):expandL f xs ys

scanL f e s = (scan_seq, scan_last)
    where   (scan_seq, scan_last) = (scanL' f e s) ||| (reduceL f e s)
            scanL' f e []  = []
            scanL' f e [x] = [e]
            scanL' f e s   = let l = scanL' f e (contractL f s) in (expandL f l s)


-- pp x y = x++"+"++y
-- ppp x y = "("++x++"+"++y++")"



{- 
    -- Actually worse
   
tabulateLAux f l r  | (r - l) < 1 = []
                    | otherwise = let mid = div (r + l) 2
                                    (l',r') = tabulateLAux f l mid ||| tabulateLAux f (mid+1) r
                                in l' ++ [f mid] ++ r'
 
tabulateL f n   | n <= 0 = []
                | otherwise = tabulateLAux f 0 n

-}
