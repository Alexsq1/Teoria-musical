module Util where

import Data.List


index :: Eq a => [a] -> a -> Int
index = index2 0
    where
        index2 _ [] _ = -1
        index2 n (x:xs) a
            | a == x = n
            | otherwise = index2 (n+1) xs a

member :: Eq a => [a] -> a -> Bool
member xs e = index xs e /= (-1)

int_division :: Int -> Int -> (Int, Int)
int_division a b
    | b == 0 = error "Division by 0"
    | a > 0 = (q * signum(b) , r)
    | otherwise = (-1 * signum(b) * (q+1), (abs (b)-r))
        where 
            q = quot (abs a) (abs b)
            r = mod (abs a) (abs b)

circularDesp :: Eq a => Int -> [a] -> [a]
circularDesp n xs = t ++ h
    where
        (_, nmod) = int_division n (length xs)
        h = take nmod xs
        t = xs \\ h
