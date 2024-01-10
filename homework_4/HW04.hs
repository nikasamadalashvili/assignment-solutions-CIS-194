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

mzip :: a -> (a -> a -> a) -> [a] -> [a] -> [a]
mzip d f (n : ns) (y : ys) = f n y : mzip d f ns ys
mzip d f [] (y : ys) = f d y : mzip d f [] ys
mzip d f (n : ns) [] = f n d : mzip d f ns []
mzip _ _ _ _ = []

plus :: (Num a) => Poly a -> Poly a -> Poly a
plus (P f) (P s) = P $ mzip 0 (+) f s

-- Exercise 5 -----------------------------------------

times :: (Num a) => Poly a -> Poly a -> Poly a
times (P f) (P s) = sum $ zipWith (\d y -> P $ replicate d 0 ++ map (* y) s) [0 ..] f

-- Exercise 6 -----------------------------------------

instance (Num a) => Num (Poly a) where
  (+) = plus
  (*) = times
  negate (P f) = P $ map (0 -) f
  fromInteger :: (Num a) => Integer -> Poly a
  fromInteger n = P [fromInteger n]

  -- No meaningful definitions exist
  abs = undefined
  signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: (Num a) => Poly a -> a -> a
applyP (P f) n = sum $ zipWith (\d y -> y * product (replicate d n)) [0 ..] f

-- Exercise 8 -----------------------------------------

class (Num a) => Differentiable a where
  deriv :: a -> a
  nderiv :: Int -> a -> a
  nderiv 1 y = deriv y
  nderiv n y = nderiv (n - 1) y

-- Exercise 9 -----------------------------------------

instance (Num a) => Differentiable (Poly a) where
  deriv (P f) = case diff of
    [] -> P [0]
    _ -> P diff
    where
      diff = zipWith (\m n -> fromInteger m * n) [1 ..] (tail f)