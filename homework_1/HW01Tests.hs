-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests =
  [ Test
      "lastDigit test"
      testLastDigit
      [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)],
    Test
      "dropLastDigit test"
      testDropLastDigit
      [(123, 12), (1234, 123), (5, 0), (10, 1), (0, 0)]
  ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

testToDigits :: (Integer, [Integer]) -> Bool
testToDigits (n, d) = toDigits n == d

ex2Tests :: [Test]
ex2Tests =
  [ Test
      "toRevDigits test"
      testToRevDigits
      [(123, [3, 2, 1]), (642535, [5, 3, 5, 2, 4, 6]), (0, []), (-241, [])],
    Test
      "toRevDigits test"
      testToRevDigits
      [(123, [3, 2, 1]), (642535, [5, 3, 5, 2, 4, 6]), (0, []), (-241, [])]
  ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (z, e) = doubleEveryOther z == e

ex3Tests :: [Test]
ex3Tests =
  [ Test
      "doubleEveryOther test"
      testDoubleEveryOther
      [([1, 2, 3, 4, 5], [1, 4, 3, 8, 5]), ([1, 2, 3, 4], [1, 4, 3, 8]), ([1, 2], [1, 4]), ([1], [1]), ([], [])]
  ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (z, r) = sumDigits z == r

ex4Tests :: [Test]
ex4Tests =
  [ Test
      "sumDigits test"
      testSumDigits
      [([1, 2, 3, 4, 5], 15), ([32, 24], 11), ([], 0), ([5591], 20), ([10, 5, 18, 4], 19)]
  ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (d, r) = luhn d == r

ex5Tests :: [Test]
ex5Tests =
  [ Test
      "luhn test"
      testLuhn
      [(341528278912561, True), (1234567898765432, False), (0, True), (10, False), (370627019108523, True), (5594589764218857, False)]
  ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, a, b, c, r) = hanoi n a b c == r

ex6Tests :: [Test]
ex6Tests =
  [ Test
      "hanoi test"
      testHanoi
      [(1, "a", "b", "c", [("a", "c")]), (0, "a", "b", "c", []), (3, "a", "b", "c", [("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c")])]
  ]

-- Exercise 7 -----------------------------------------

testHanoiFour :: (Integer, Peg, Peg, Peg, Peg, Int) -> Bool
testHanoiFour (n, a, b, c, d, r) = length (hanoiFour n a b c d) == r

ex7Tests :: [Test]
ex7Tests =
  [ Test
      "hanoiFour test"
      testHanoiFour
      [(1, "a", "b", "c", "d", 1), (0, "a", "b", "c", "d", 0), (9, "a", "b", "c", "d", 41), (32, "a", "b", "c", "d", 1281)]
  ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests =
  concat
    [ ex1Tests,
      ex2Tests,
      ex3Tests,
      ex4Tests,
      ex5Tests,
      ex6Tests,
      ex7Tests
    ]
