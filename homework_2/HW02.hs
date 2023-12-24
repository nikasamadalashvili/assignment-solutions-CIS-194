{-# OPTIONS_GHC -Wall #-}

module HW02 where

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
exactMatches a g = sum [1 | (f, s) <- zip a g, f == s]

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ps = map (\x -> length $ filter (== x) ps) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches ac gc = sum $ zipWith min (countColors ac) (countColors gc)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact nonExact
  where
    exact = exactMatches s g
    nonExact = matches s g - exact

-- Exercise 4 -----------------------------------------
getCode :: Move -> Code
getCode (Move code _ _) = code

isConsistent :: Move -> Code -> Bool
isConsistent m c = m == getMove c (getCode m)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter $ isConsistent m

-- Exercise 6 -----------------------------------------

allCodesFrom :: [Code] -> [Code]
-- the same implementation using list comprehension
-- allCodesFrom codes = [color : code | code <- codes, color <- colors]
allCodesFrom = concatMap (\code -> map (: code) colors)

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
allCodes n = allCodesFrom $ allCodes (n - 1)

-- Exercise 7 -----------------------------------------

solveRec :: Code -> [Code] -> [Move]
solveRec _ [] = []
solveRec secret (next : gs)
  | nextCode == secret = [getMove secret nextCode]
  | otherwise = nextMove : solveRec secret availableCodes
  where
    nextMove = getMove secret next
    nextCode = getCode nextMove
    availableCodes = filterCodes nextMove gs

solve :: Code -> [Move]
solve secret = solveRec secret $ allCodes $ length secret

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
