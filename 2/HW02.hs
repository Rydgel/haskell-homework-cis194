{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- http://www.seas.upenn.edu/~cis194/hw/02-lists.pdf

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess

exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (uncurry (==)) $ zip c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\c -> length $ filter (==c) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact (allMatches-exact)
  where exact = exactMatches s g
        allMatches = matches s g

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c e a) c' = e == e' && a == a'
  where (Move _ e' a') = getMove c c'

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [ c : sc | c <- colors, sc <- allCodes (n - 1) ]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = choose c $ allCodes 6

choose :: Code -> [Code] -> [Move]
choose _ [] = []
choose c (x:xs) =
    if x == c then [m] else m : choose c (filterCodes m xs)
    where m = getMove c x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
