module HW03 where

data Expression
  = Var String -- Variable
  | Val Int -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement
  = Assign String Expression
  | Incr String
  | If Expression Statement Statement
  | While Expression Statement
  | For Statement Expression Statement Statement
  | Sequence Statement Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state target targetValue t
  | t == target = targetValue
  | otherwise = state t

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------
evalBOp :: Int -> Bop -> Int -> Int
evalBOp val1 bOp val2 = case bOp of
  Plus -> val1 + val2
  Minus -> val1 - val2
  Times -> val1 * val2
  Divide -> val1 `div` val2
  Gt -> toInt $ val1 > val2
  Ge -> toInt $ val1 >= val2
  Lt -> toInt $ val1 < val2
  Le -> toInt $ val1 <= val2
  Eql -> toInt $ val1 == val2
  where
    toInt b
      | b = 1
      | otherwise = 0

evalE :: State -> Expression -> Int
evalE state expression = case expression of
  (Var varName) -> state varName
  (Val varValue) -> varValue
  (Op ex1 bOp ex2) -> evalBOp (evalE state ex1) bOp (evalE state ex2)

-- Exercise 3 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar stmt = case stmt of
  Assign n e -> DAssign n e
  Incr n -> DAssign n (Op (Var n) Plus (Val 1))
  If e s1 s2 -> DIf e (desugar s1) (desugar s2)
  While e s -> DWhile e (desugar s)
  For s1 e s2 s3 -> DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2)))
  Sequence s1 s2 -> DSequence (desugar s1) (desugar s2)
  Skip -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state dStmt = case dStmt of
  (DAssign n e) -> extend state n (evalE state e)
  (DIf e ds1 ds2)
    | evalE state e == 0 -> evalSimple state ds2
    | otherwise -> evalSimple state ds1
  dW@(DWhile e ds)
    | evalE state e == 0 -> state
    | otherwise -> evalSimple state (DSequence ds dW)
  (DSequence ds1 ds2) -> evalSimple (evalSimple state ds1) ds2
  DSkip -> state

run :: State -> Statement -> State
run state st = evalSimple state (desugar st)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial =
  For
    (Assign "Out" (Val 1))
    (Op (Var "In") Gt (Val 0))
    (Assign "In" (Op (Var "In") Minus (Val 1)))
    (Assign "Out" (Op (Var "In") Times (Var "Out")))

{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot =
  slist
    [ Assign "B" (Val 0),
      While
        (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
        (Incr "B"),
      Assign "B" (Op (Var "B") Minus (Val 1))
    ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci =
  slist
    [ Assign "F0" (Val 1),
      Assign "F1" (Val 1),
      If
        (Op (Var "In") Eql (Val 0))
        (Assign "Out" (Var "F0"))
        ( If
            (Op (Var "In") Eql (Val 1))
            (Assign "Out" (Var "F1"))
            ( For
                (Assign "C" (Val 2))
                (Op (Var "C") Le (Var "In"))
                (Incr "C")
                ( slist
                    [ Assign "T" (Op (Var "F0") Plus (Var "F1")),
                      Assign "F0" (Var "F1"),
                      Assign "F1" (Var "T"),
                      Assign "Out" (Var "T")
                    ]
                )
            )
        )
    ]