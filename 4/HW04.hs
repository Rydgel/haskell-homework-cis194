{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P [] == P []         = True
    P [] == P (a:as)     = a == 0 && P [] == P as
    P a == P []          = P [] == P a
    P (a:as) == P (b:bs) = a == b && P as == P bs

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = "0"
  show (P a)  = foldl myConcat "" $ map termToString $ toTerm $ reverse a
    where
      toTerm []     = []
      toTerm (b:bs) = (b, length bs) : toTerm bs

      termToString (0, _) = ""
      termToString (b, 0) = show b
      termToString (b, c) = prefix b ++ "x" ++ suffix c

      prefix 1    = ""
      prefix (-1) = "-"
      prefix b    = show b

      suffix 1 = ""
      suffix b = "^" ++ show b

      myConcat b "" = b
      myConcat "" c = c
      myConcat b c  = b ++ " + " ++ c

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ add a b
  where
    add [] y' = y'
    add x' [] = x'
    add (x':xs) (y':ys) = x' + y' : add xs ys

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = P $ timesImpl p1 p2 where
	timesImpl _ [] = []
	timesImpl xs (hy:ys) = result where
		(P result) = plus (P [t * hy | t <- xs]) (P (timesImpl (0:xs) ys))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P $ map negate a
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) num = applyImpl p 1 where
	applyImpl [] _ = 0
	applyImpl (hx:xs) n = hx * n + applyImpl xs (n * num)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 1 d = deriv d
    nderiv n d = nderiv (n-1) (deriv d)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
  deriv (P a) = P $ reverse $ deriv' $ reverse a
    where
      deriv' [ ] = []
      deriv' [_] = []
      deriv' (y:ys) = y * fromIntegral (length ys) : deriv' ys
