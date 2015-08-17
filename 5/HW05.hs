{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import           Control.Applicative ((<$>))
import           Data.List
import           Data.Function (on)

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orgn modf = do
  orgnStr <- BS.readFile orgn
  modfStr <- BS.readFile modf
  return $ BS.pack [x `xor` y | (x, y) <- BS.zip orgnStr modfStr, x /= y]

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k f = do
  fileStr <- BS.readFile $ f ++ ".enc"
  BS.writeFile f $ BS.pack [x `xor` y | (x, y) <- BS.zip fileStr (BS.cycle k)]

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = decode <$> BS.readFile f

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs v t = do
  victims <- parseFile v :: IO (Maybe [TId])
  transactions <- parseFile t :: IO (Maybe [Transaction])
  let isElement (Just vs) tx = tid tx `elem` vs
      isElement Nothing   _  = False
  return $ filter (isElement victims) <$> transactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow =
  foldr (\t m ->
    Map.insertWith (-) (from t) (amount t) $
    Map.insertWith (+) (to t) (amount t) m) Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.foldrWithKey (\x e r-> if e > snd r then (x,e) else r) ("",0) m

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = map makeT (zip3 sortedPayers sortedPayees tids)
  where payers = Map.toList $ Map.filter (> 0) m
        payees = Map.toList $ Map.filter (<= 0) m
        sortedPayers = sortBy (flip compare `on` snd) payers
        sortedPayees = sortBy (compare `on` snd) payees
        makeT (py,pee,t) = Transaction { from = fst py
                                       , to = fst pee
                                       , amount = snd py - snd pee
                                       , tid = t
                                       }

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f x = BS.writeFile f (encode x)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
