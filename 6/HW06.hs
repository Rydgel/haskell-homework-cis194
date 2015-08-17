{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Main where

import Data.List
import Data.Functor ()

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f s = Cons s $ sIterate f (f s)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x1 xs1) (Cons x2 xs2) = Cons x1 $ Cons x2 $ sInterleave xs1 xs2

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave d $ sInterleave a $ sInterleave b c
  where a = sRepeat 1
        b = sRepeat 2
        c = sIterate (+1) 3
        d = sRepeat 0

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 40 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 40k -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = go Nothing where
    go m []      = m
    go !m (x:xs) = go (case m of
            Nothing -> Just (x, x)
            Just (mi, ma)
                | x < mi -> Just (x, ma)
                | x > ma -> Just (mi, x)
                | otherwise -> Just (mi, ma)
        ) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Row = Row Integer Integer
data Matrix = Matrix Row Row

instance Num (Matrix) where
    (*) (Matrix (Row xa xb) (Row xc xd)) (Matrix (Row ya yb) (Row yc yd)) =
      Matrix (Row (xa*ya+xb*yc) $ xa*yb+xb*yd) (Row (xc*ya+xd*yc) $ xc*yb+xd*yd)

extractFibFromMatrix :: Matrix -> Integer
extractFibFromMatrix (Matrix (Row _ b) _) = b

fastFib :: Int -> Integer
fastFib 0 = 0
fastFib n = extractFibFromMatrix (Matrix (Row 1 1) (Row 1 0)^n)
