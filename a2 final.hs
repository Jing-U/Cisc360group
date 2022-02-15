-- CISC 360 a2, Winter 2022
-- Jana Dunfield

-- SEE THE FILE a2.pdf
-- for instructions

module A2 where
import Data.Char

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 20182972     -- 20155672

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.


{-
   Q2: rewrite
-}
divisible_by :: Int -> Char -> Bool
divisible_by factor ch = (mod (ord ch) factor == 0)

rewrite :: (Char -> Bool) -> String -> String
rewrite boring []       = []
rewrite boring (c : cs) = 
  if boring c == False then (c : rewrite boring cs)
  else rewrite boring cs

test_rewrite1 = (rewrite (divisible_by 2) "Queen's") == "Quee's"
test_rewrite2 = (rewrite (\x -> x == ' ') "take it in tow" == "takeitintow")
test_rewrite3 = (rewrite (\c -> (c > 'a')) "sIlLy CaPiTaLiZaTiOn") == "IL CaPTaLZaTO"


{-
  Q3: lists
-}

{-
  Q3a. Fill in the definition of listCompare.
  See a2.pdf for instructions.
-}
listCompare :: ([Integer], [Integer]) -> [Bool]

listCompare ([],   []  )   = []
listCompare (x:xs, y:ys)   = 
  if x == y then [False] ++ listCompare (xs, ys)
  else [True] ++ listCompare (xs, ys)
listCompare (x:xs, []  )   = [False] ++ listCompare (xs, [])
listCompare ([],   y:ys)   = [False] ++ listCompare (ys, [])

test_listCompare1 = listCompare ([1, 2, 4], [3, 2, 0]) == [True, False, True]
test_listCompare2 = listCompare ([1, 2, 1, 1], [1, 5]) == [False, True, False, False]
test_listCompare3 = listCompare ([1, 1], [1, 1, 1, 1]) == [False, False, False, False]


{-
  Q3b.
  Briefly explain why listCompare cannot
  be implemented by

    listCompare :: ([Integer], [Integer]) -> [Bool]
    listCompare (xs, ys) = zipWith (/=) xs ys

  Write your brief explanation here:
  
  If I implete the listCompare by "listCompare (xs, ys) = zipWith (/=) xs ys",
  there will not be correct for all the situations. Because zipWith cannot deal with
  the situation that xs and ys are not the same length. If xs is longer than ys, the 
  part that xs exceed to ys will not be regarded.
  For example, if the input of two lists are [1,1,1] and [2,2], the expected result is
  [False,False,False], but the actual result is [False, False].
  Thus the zipWith cannot be used here.
-}


{-
  Q3c. Fill in the definition of polyCompare.
  See a2.pdf for instructions.
-}
polyCompare :: (a -> a -> Bool, [a], [a]) -> [Bool]
polyCompare (cmp, [],   []  )   = []
polyCompare (cmp, x:xs, y:ys)   = [cmp x y] ++ polyCompare (cmp, xs, ys)
polyCompare (cmp, x:xs, []  )   = [False] ++ polyCompare (cmp, xs, [])
polyCompare (cmp, [],   y:ys)   = [False] ++ polyCompare (cmp, [], ys)

test_polyCompare1 = polyCompare (\i -> \j -> i /= j, [1, 2, 4], [3, 2, 0])
                    == [True, False, True]

-- whoever calls polyCompare gets to define what "different" means:
--  in test_polyCompare2, the definition of "different" becomes whether two lists (here, strings) have different lengths, regardless of the lists' contents
lengthsEqual :: [a] -> [a] -> Bool
lengthsEqual xs ys = (length xs /= length ys)
test_polyCompare2 = polyCompare (lengthsEqual, ["a", "ab", "abcd"], ["ccc", "xy", ""])
                    == [True, False, True]


{-
  Q4. Songs
-}

data Song = Harmony Song Song
          | Pitch Integer
          | Stop
          deriving (Show, Eq)    -- writing Eq here lets us use == to compare Songs
          
{-
  Q4. sing: See a2.pdf for complete instructions.

  The idea of 'sing' is to apply the "harmonizing rule" as much as possible: wherever the tree has

      Harmony
      /     \
  Pitch m   Pitch n

  replace that with  Pitch (m * n).  Repeat until the harmonizing rule cannot be applied anywhere.

  For example:

    sing (Harmony (Harmony (Pitch 2) (Pitch 10)) (Pitch 6))

  should return

    (Pitch 120)

  because:

  - harmonizing (Harmony (Pitch 2) (Pitch 10)) should give (Pitch 20), and
  - harmonizing (Harmony (Pitch 20) (Pitch 6)) should give (Pitch 120).

  sing (Harmony Stop (Harmony (Pitch 2) (Pitch 4)))

  should return

     Harmony Stop (Pitch 8)

  because we can harmonize (Harmony (Pitch 2) (Pitch 4)) into Pitch 8, but the Stop cannot be harmonized.

  You will probably need to write at least one helper function.
  Think about whether we can break the problem down into
  a helper function that applies the harmonizing rule *once*, or "a few times",
  and then have 'sing' call that function.
-}
sing :: Song -> Song
sing (Stop) = Stop
sing (Pitch n) = Pitch n
sing (Harmony (Pitch n) (Pitch m)) = Pitch (n*m)
sing (Harmony Stop right) = help (Harmony Stop (sing right))
sing (Harmony left Stop) = help (Harmony (sing left) Stop)
sing (Harmony (Pitch n) right) = help (Harmony (Pitch n) (sing right))
sing (Harmony left (Pitch n)) = help (Harmony (sing left) (Pitch n))
sing (Harmony left right) = sing (Harmony (sing left) (sing right))

help:: Song -> Song
help (Harmony (Pitch n) (Pitch m)) = Pitch (n*m)
help (Harmony Stop right) = Harmony Stop (sing right)
help (Harmony left Stop) = Harmony (sing left) Stop
help (Harmony (Pitch n) right) = Harmony (Pitch n) (sing right)
help (Harmony left (Pitch n)) = Harmony (sing left) (Pitch n)


p2 = Pitch 2
p6 = Pitch 6
p10 = Pitch 10
a1 = (Harmony (Harmony p2 p6) (Harmony Stop p10))
test_sing1 = sing (Harmony (Harmony p2 p10) (Pitch 6)) ==
             Pitch 120
test_sing2 = sing (Harmony Stop (Harmony p2 (Pitch 4))) ==
             Harmony Stop (Pitch 8)
test_sing3 = sing (Harmony (Harmony (Harmony p2 p6) (Harmony Stop p10)) p6) ==
             Harmony (Harmony (Pitch 12) (Harmony Stop p10)) p6
test_sing4 = sing (Harmony (Harmony (Harmony p2 p6) (Harmony p10 p10)) p6) ==
             Pitch 7200

test_sing = test_sing1 && test_sing2 && test_sing3 && test_sing4

