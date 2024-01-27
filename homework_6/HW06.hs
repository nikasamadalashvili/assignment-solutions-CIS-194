{-# OPTIONS_GHC -Wall #-}

module HW06 where

import Data.Functor ()
import Data.List (foldl', intercalate)

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance (Show a) => Show (Stream a) where
  show s =
    "["
      ++ intercalate ", " (map show $ take 10 $ streamToList s)
      ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f $ f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x $ sInterleave y xs

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs)
  | n > 0 = x : sTake (n - 1) xs
  | otherwise = []

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+ 1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) ((+ 1) <$> ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r0 = Cons rn $ rand rn
  where
    rn = (1103515245 * fromIntegral r0 + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x : xs) = Just $ foldl' updateMinMax' (x, x) xs
  where
    updateMinMax' (!mi, !ma) y = (min mi y, max ma y)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show)

instance Num Matrix where
  (+) = undefined
  (*) :: Matrix -> Matrix -> Matrix
  (*) (Matrix x00 x01 x10 x11) (Matrix y00 y01 y10 y11) = Matrix z00 z01 z10 z11
    where
      z00 = x00 * y00 + x01 * y10
      z01 = x00 * y01 + x01 * y11
      z10 = x10 * y00 + x11 * y10
      z11 = x10 * y01 + x11 * y11
  abs = undefined
  signum = undefined
  fromInteger :: Integer -> Matrix
  fromInteger = undefined
  negate = undefined

mainMatrix :: Matrix
mainMatrix = Matrix 1 1 1 0

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = fn
  where
    (Matrix fn _ _ _) = mainMatrix ^ n
