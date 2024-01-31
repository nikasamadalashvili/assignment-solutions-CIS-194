{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module HW07 where

import Cards (Card (..), Deck, labels, suits)
import Control.Monad (liftM2, replicateM)
import Control.Monad.Random
  ( MonadRandom (getRandom, getRandomR),
    Rand,
    Random,
    StdGen,
    evalRandIO,
  )
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import Prelude hiding (mapM)

-- Exercise 1 -----------------------------------------

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM = (<$>)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = liftM2 (\v1 v2 -> v // [(i1, v2), (i2, v1)]) (v !? i1) (v !? i2)

-- Exercise 2 -----------------------------------------

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x : xs) = do
  mb <- f x
  re <- HW07.mapM f xs
  return (mb : re)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = HW07.mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, V.length v) <&> (v !?)

-- Exercise 4 -----------------------------------------

randomVec :: (Random a) => Int -> Rnd (Vector a)
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: (Random a) => Int -> (a, a) -> Rnd (Vector a)
randomVecR n b = V.fromList <$> replicateM n (getRandomR b)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = V.foldM' (\nv i -> (\j -> fromJust (swapV i j nv)) <$> getRandomR (0, i)) v (V.enumFromStepN vLastIndex (-1) vLastIndex)
  where
    vLastIndex = V.length v - 1

-- Exercise 6 -----------------------------------------

partitionAt :: (Ord a) => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (less, t, greater)
  where
    t = v ! i
    (less, greater) = V.partition (< t) (V.ifilter (\ix _ -> ix /= i) v)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  quicksort [y | y <- xs, y < x]
    <> (x : quicksort [y | y <- xs, y >= x])

qsort :: (Ord a) => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = qsort l <> return t <> qsort r
  where
    (l, t, r) = partitionAt v 0

-- Exercise 8 -----------------------------------------

qsortR :: (Ord a) => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = do
      pivot <- getRandomR (0, V.length v - 1)
      let (l, t, r) = partitionAt v pivot
      lSorted <- qsortR l
      rSorted <- qsortR r
      return $ lSorted <> V.singleton t <> rSorted

-- Exercise 9 -----------------------------------------

-- Selection
select :: (Ord a) => Int -> Vector a -> Rnd (Maybe a)
select i v
  | i < 0 || i >= V.length v = return Nothing
  | otherwise =
      getRandomR (0, V.length v - 1) >>= \pivot ->
        let (l, t, r) = partitionAt v pivot
         in case compare i (V.length l) of
              LT -> select i l
              GT -> select (i - V.length l - 1) r
              _ -> return (Just t)

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = V.uncons

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 d = Just ([], d)
getCards n d = do
  (nc, nd) <- nextCard d
  (cs, rd) <- getCards (n - 1) nd
  return (nc : cs, rd)

-- Exercise 13 ----------------------------------------

data State = State {money :: Int, deck :: Deck}

repl :: State -> IO ()
repl s@State {..}
  | money <= 0 = putStrLn "You ran out of money!"
  | V.null deck = deckEmpty
  | otherwise = do
      putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
      putStrLn "Would you like to play (y/n)?"
      cont <- getLine
      if cont == "n"
        then
          putStrLn $
            "You left the casino with \ESC[32m$"
              ++ show money
              ++ "\ESC[0m"
        else play
  where
    deckEmpty =
      putStrLn $
        "The deck is empty. You got \ESC[32m$"
          ++ show money
          ++ "\ESC[0m"
    play = do
      putStrLn "How much do you want to bet?"
      amt <- read <$> getLine
      if amt < 1 || amt > money
        then play
        else do
          case getCards 2 deck of
            Just ([c1, c2], d) -> do
              putStrLn $ "You got:\n" ++ show c1
              putStrLn $ "I got:\n" ++ show c2
              case () of
                _
                  | c1 > c2 -> repl $ State (money + amt) d
                  | c1 < c2 -> repl $ State (money - amt) d
                  | otherwise -> war s {deck = d} amt
            _ -> deckEmpty
    war (State m d) amt = do
      putStrLn "War!"
      case getCards 6 d of
        Just ([c11, c21, c12, c22, c13, c23], d') -> do
          putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
          putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
          case () of
            _
              | c13 > c23 -> repl $ State (m + amt) d'
              | c13 < c23 -> repl $ State (m - amt) d'
              | otherwise -> war (State m d') amt
        _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
