{-# OPTIONS_GHC -Wall #-}

module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: (Num a) => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  (==) :: (Num a, Eq a) => Poly a -> Poly a -> Bool
  P xs == P ys = removeTrailingZeros xs == removeTrailingZeros ys
    where
      removeTrailingZeros = reverse . dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show :: (Num a, Eq a, Show a) => Poly a -> String
  show (P xs) = case result of
    "" -> "0"
    _ -> drop 3 result
    where
      result = getStr 0 xs
      getStr :: Int -> [a] -> String
      getStr _ [] = ""
      getStr d (y : ys)
        | y == 0 = getStr (d + 1) ys
        | y == 1 && d > 1 = getStr (d + 1) ys ++ " + " ++ "x^" ++ show d
        | y == -1 && d > 1 = getStr (d + 1) ys ++ " + " ++ "-x^" ++ show d
        | y == 1 && d == 1 = getStr (d + 1) ys ++ " + " ++ "x"
        | y == -1 && d == 1 = getStr (d + 1) ys ++ " + " ++ "-x"
        | d == 0 = getStr (d + 1) ys ++ " + " ++ show y
        | d == 1 = getStr (d + 1) ys ++ " + " ++ show y ++ "x"
        | otherwise = getStr (d + 1) ys ++ " + " ++ show y ++ "x^" ++ show d

-- Exercise 4 -----------------------------------------

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: (Num a) => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance (Num a) => Num (Poly a) where
  (+) = plus
  (*) = times
  negate = undefined
  fromInteger = undefined

  -- No meaningful definitions exist
  abs = undefined
  signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: (Num a) => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class (Num a) => Differentiable a where
  deriv :: a -> a
  nderiv :: Int -> a -> a
  nderiv = undefined

-- Exercise 9 -----------------------------------------

instance (Num a) => Differentiable (Poly a) where
  deriv = undefined
