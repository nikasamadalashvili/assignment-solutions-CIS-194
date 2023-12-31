-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module HW02Tests where

import HW02
import Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests =
  [ testF2
      "exactMatches test"
      exactMatches
      [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0),
        ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
      ]
  ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests =
  [ testF1
      "countColors test"
      countColors
      [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1]),
        ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
      ],
    testF2
      "matches test"
      matches
      [([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)]
  ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests =
  [ testF2
      "getMove test"
      getMove
      [ ( [Red, Blue, Yellow, Orange],
          [Red, Orange, Orange, Blue],
          Move [Red, Orange, Orange, Blue] 1 2
        )
      ]
  ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests =
  [ testF2
      "isConsistent test"
      isConsistent
      [ ( Move [Red, Red, Blue, Green] 1 1,
          [Red, Blue, Yellow, Purple],
          True
        ),
        ( Move [Red, Red, Blue, Green] 1 1,
          [Red, Blue, Red, Purple],
          False
        )
      ]
  ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests =
  [ testF2
      "filterCodes test"
      filterCodes
      [ ( Move [Red, Red, Blue, Green] 1 1,
          [[Red, Red, Yellow, Purple], [Red, Blue, Purple, Purple]],
          [[Red, Blue, Purple, Purple]]
        ),
        ( Move [Red, Blue, Red, Green] 2 2,
          [[Red, Blue, Green, Red], [Red, Blue, Red, Purple], [Blue, Red, Red, Green]],
          [[Red, Blue, Green, Red], [Blue, Red, Red, Green]]
        )
      ]
  ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests =
  [ testF1
      "allCodes test"
      allCodes
      [ ( 2,
          [ [Red, Red],
            [Green, Red],
            [Blue, Red],
            [Yellow, Red],
            [Orange, Red],
            [Purple, Red],
            [Red, Green],
            [Green, Green],
            [Blue, Green],
            [Yellow, Green],
            [Orange, Green],
            [Purple, Green],
            [Red, Blue],
            [Green, Blue],
            [Blue, Blue],
            [Yellow, Blue],
            [Orange, Blue],
            [Purple, Blue],
            [Red, Yellow],
            [Green, Yellow],
            [Blue, Yellow],
            [Yellow, Yellow],
            [Orange, Yellow],
            [Purple, Yellow],
            [Red, Orange],
            [Green, Orange],
            [Blue, Orange],
            [Yellow, Orange],
            [Orange, Orange],
            [Purple, Orange],
            [Red, Purple],
            [Green, Purple],
            [Blue, Purple],
            [Yellow, Purple],
            [Orange, Purple],
            [Purple, Purple]
          ]
        )
      ]
  ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests =
  [ testF1
      "solve test"
      solve
      [ ( [Yellow, Orange, Red, Red],
          [ Move [Red, Red, Red, Red] 2 0,
            Move [Green, Green, Red, Red] 2 0,
            Move [Blue, Blue, Red, Red] 2 0,
            Move [Yellow, Yellow, Red, Red] 3 0,
            Move [Orange, Yellow, Red, Red] 2 2,
            Move [Yellow, Orange, Red, Red] 4 0
          ]
        )
      ]
  ]

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = []

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
      ex7Tests,
      bonusTests
    ]
