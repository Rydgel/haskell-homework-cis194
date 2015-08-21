{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random ()

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y v = do
  maybeX <- v !? x
  maybeY <- v !? y
  return $ v // [(x,maybeY), (y,maybeX)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
    b <- f x
    bs <- mapM f xs
    return $ b : bs

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs v = mapM (v !?) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  i <- getRandomR (0, V.length v - 1)
  return $ v !? i

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (lo, hi) = liftM V.fromList $ replicateM n $ getRandomR (lo, hi)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = shuffle' v (V.length v - 1)
  where shuffle' v' 0 = return v'
        shuffle' v' i = do
          j <- getRandomR (0, i)
          case swapV i j v' of
            Nothing -> return V.empty
            Just v'' -> shuffle' v'' (i - 1)


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v n = (vFirst, v1, vLast)
  where v1 = v ! n
        v' = V.ifilter (\j _ -> j /= n) v
        vFirst = V.filter (< v1) v'
        vLast = V.filter (>= v1) v'

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v =
  if V.null v then V.empty
  else qsort [ y | y <- xs, y < x ]
    <> cons x (qsort [y | y <- xs, y >= x ])
      where (x, xs) = (V.head v, V.tail v)


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v =
  if V.null v then return V.empty
  else do
    pivot <- getRandomR (0, V.length v - 1)
    let (vless, vi, vgreat) = partitionAt v pivot
    vlessR <- qsortR vless
    vgreatR <- qsortR vgreat
    return $ vlessR V.++ cons vi vgreatR

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | V.null v = return Nothing
  | otherwise = do
      r <- getRandomR (0, V.length v - 1)
      let (lt, p, gt) = partitionAt v r
      case compare i (V.length lt) of
        LT -> select i lt
        EQ -> return $ Just p
        GT -> select (i - V.length lt - 1) gt

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card y x | x <- suits, y <- labels]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard v
  | V.null v  = Nothing
  | otherwise = Just (V.head v, V.tail v)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 d = Just ([], d)
getCards n d = do
  (c, d') <- nextCard d
  (cs, d'') <- getCards (n - 1) d'
  return (c : cs, d'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
