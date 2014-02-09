{-# LANGUAGE BangPatterns #-}

module Utils where

mapi :: Num n => (n -> a -> b) -> [a] -> [b]
mapi f = go 0
  where go _ []     = []
        go !i (x:xs) = f i x : go (i + 1) xs

mapi' f xs = zipWith f [0..] xs



(&) :: a -> (a -> b) -> b
x & f = f x

