{- Implementación del TAD secuencia -}

module ListSeq where

import Seq

-- ~ NONOPTIMAL, JUST PLAYING ~ --
    
instance Seq [] where
    emptyS          = []
    singletonS x    = [x]
    lengthS x       = length x
    nthS x i        = x !! i
    tabulateS f n   = map f [0..n-1]
    mapS f x        = map f x
    filterS p x     = filter p x
    appendS x y     = x ++ y
    takeS x n       = take n x
    dropS x n       = drop n x
    -- ~ showtS x        =
    -- ~ showlS x        =
    joinS x         = concat x
    -- ~ reduceS f e x   =         
    -- ~ scan f e x      =
    fromList x      = x

-- ~ NONOPTIMAL, JUST PLAYING ~ --
