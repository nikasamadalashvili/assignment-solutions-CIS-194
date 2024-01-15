{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HW05 where

import Data.Bits (Bits (xor))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.List (sort, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set
import Parser (FromJSON, TId, ToJSON, Transaction (..), decode, encode)
import System.Environment (getArgs)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalPath modifiedPath = do
  originalFile <- BS.readFile originalPath
  modifiedFile <- BS.readFile modifiedPath
  return $ BS.filter (/= 0) $ BS.pack $ BS.zipWith xor originalFile modifiedFile

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encryptedData <- BS.readFile $ path ++ ".enc"
  BS.writeFile path $ BS.pack $ BS.zipWith xor encryptedData $ BS.cycle key

-- Exercise 3 -----------------------------------------

parseFile :: (FromJSON a) => FilePath -> IO (Maybe a)
parseFile filePath = decode <$> BS.readFile filePath

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  victims <- parseFile victimsPath :: IO (Maybe [TId])
  transactions <- parseFile transactionsPath :: IO (Maybe [Transaction])
  return $ filterTransactions <$> victims <*> transactions
  where
    filterTransactions :: [TId] -> [Transaction] -> [Transaction]
    filterTransactions victims = filter (\t -> tid t `Set.member` victimsSet)
      where
        victimsSet = Set.fromList victims

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = Map.fromListWith (+) $ concatMap (\(Transaction f t a _) -> [(f, -a), (t, a)]) ts

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flowMap = maybe "" fst (Map.lookupMax flowMap)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flowMap ids
  | Map.null flowMap = []
  | otherwise = map fst items ++ undoTs (Map.fromList $ drop dropSize payees ++ drop dropSize payers ++ concatMap (\(_, (f, t)) -> [f, t]) items) newIds
  where
    items = zipWith3 makeReverseT payers payees ids
    payers = sortBy (comparing (Down . snd)) $ filter ((> 0) . snd) $ Map.toList flowMap
    payees = sort $ filter ((< 0) . snd) $ Map.toList flowMap
    dropSize = min (length payers) (length payees)
    newIds = drop (length items) ids
    makeReverseT :: (String, Integer) -> (String, Integer) -> TId -> (Transaction, ((String, Integer), (String, Integer)))
    makeReverseT (fN, fB) (tN, tB) tId = (Transaction fN tN amount tId, ((fN, fB - amount), (tN, tB + amount)))
      where
        amount = min fB (-tB)

-- Exercise 8 -----------------------------------------

writeJSON :: (ToJSON a) => FilePath -> a -> IO ()
writeJSON path d = BS.writeFile path $ encode d

-- Exercise 9 -----------------------------------------

doEverything ::
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1 : dog2 : trans : vict : ids : out : _ ->
        doEverything dog1 dog2 trans vict ids out
      _ ->
        doEverything
          "dog-original.jpg"
          "dog.jpg"
          "transactions.json"
          "victims.json"
          "new-ids.json"
          "new-transactions.json"
  putStrLn crim
