-- exercises from the book "Introduction to Computation: Haskell, Logic and 
-- Automata"
-- consider my solutions as public domain

-- Chapter 3 - Simple Computations

import Data.Char (isDigit, toLower, toUpper)

-- ex 1
root :: Float -> Float -> Float -> Float
root a b c  = (sqrt (b ^ 2 - 4 * a * c) - b) / 2 / a

-- ex 2
hour :: Int -> Int
hour minutes = mod (div minutes 60 + 1) 12

-- ex 3
between :: Int -> Int -> Int -> Int
between x y z
 | (y <= x && x <= z) || (z <= x && x <= y) = x
 | (x <= y && y <= z) || (z <= y && y <= x) = y
 | (x <= z && z <= y) || (y <= z && z <= x) = z
{-
i also found this solution on the web:

between a b c = (sort [a, b, c]) !! 1

i haven't thought about sorting the arguments like this, and then selecting the
middle one. i found it more clever than my solution
-}

-- ex 4
xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- ex 5
ifdisj :: Bool -> Bool -> Bool
ifdisj x y = if x then True else y

gdisj :: Bool -> Bool -> Bool
gdisj x y
 | x = True
 | otherwise = y

cdisj :: Bool -> Bool -> Bool
cdisj False False = False
cdisj _ _ = True

{-
"Which definition do you think is most elegant or easiest to understand? Why?"

i believe the definition by cases is the easiest to understand. it clearly 
shows the one and only case where x || y == false. but i quite enjoy the one 
using if-else, it is a simple one-liner
-}

-- ex 6
testRounding = print [round 4.5, round (-4.4), ceiling 4.5]

-- Chapter 5 - Lists and Comprehensions
-- ex 1
vecDot :: (Float, Float) -> (Float, Float) -> Float
vecDot (x0, y0) (x1, y1) = x0 * x1 + y0 * y1

vecLen :: (Float, Float) -> Float
vecLen x = sqrt (vecDot x x)

vecAngle :: (Float, Float) -> (Float, Float) -> Float
vecAngle a b = acos (vecDot a b / vecLen a / vecLen b)

-- ex 2
type Line = (Float, Float)

intersect :: Line -> Line -> (Float, Float)
intersect (a, b) (a', b') = (x, y)
 where x = (b' - b) / (a - a')
       y = a * x + b

-- ex 3
halveEvens :: [Int] -> [Int]
halveEvens l = [ div n 2 | n <- l, even n ]

-- ex 4
inRange :: Int -> Int -> [Int] -> [Int]
inRange min max l = [ n | n <- l, n >= min, n <= max]

-- ex 5
countPositives :: [Int] -> Int
countPositives l = length [ n | n <- l, n >= 0 ]

-- a version which does not use `length`, but also no list comprehension
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (h:t)
 | h >= 0 = countPositivesRec t + 1
 | otherwise = countPositivesRec t

-- ex 6
mulList :: [Int] -> Int
mulList [] = 0
mulList [a] = a
mulList (h:t) = h * mulList t

multDigits :: String -> Int
multDigits s = mulList [ read [n] :: Int | n <- s, isDigit n ]

-- a version using foldr. this was my first solution but i think the intended
-- solution should use recursion
multDigitsFold :: String -> Int
multDigitsFold s
 | null digits = 0
 | otherwise = foldl (*) 1 digits
 where digits = [ read [n] :: Int | n <- s, isDigit n ]

-- ex 7
capitalise :: String -> String
capitalise (head:s) = toUpper head : [ toLower c | c <- s ]

-- ex 8
-- this seems extremely wrong but i guess it works
-- both this hack i did for printing the StatementType and the code for 
-- checkTautology seems wrong. i should revisit this one at a later date

data StatementType = Simple | Tautology | Contradiction
printStatement :: StatementType -> IO ()
printStatement Simple = putStr "Simple\n"
printStatement Tautology = putStr "Tautology\n"
printStatement Contradiction = putStr "Contradiction\n"

checkTautology :: StatementType
checkTautology 
 | and result = Tautology
 | or result = Simple
 | otherwise = Contradiction
 where result = [ ((and [a, not b, or [c, d && b]]) || 
                   (not b && not a)) && c | 
                    a <- [True, False], 
                    b <- [True, False], 
                    c <- [True, False], 
                    d <- [True, False] ]

-- ex 9
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind c i len words = [ w | w <- words, w !! i == c, length w == len ]

-- ex 10
pythagoreanTriples :: [(Int,Int,Int)]
pythagoreanTriples = [ (a,b,c) | a <- [1..], b <- [1..], c <- [1..], 
                                 a <= b, b < c,
                                 a ^ 2 + b ^ 2 == c ^ 2 ]

-- "What is the problem with this definition?"
-- it doesnt work :(((((((
-- i guess it is trying all numbers from 1 to infinity on a first, then trying 
-- on b and then c. but we can never reach infinity like this so it gets stuck.
-- the obvious solution is to add a range to it, but i would prefer keeping
-- the interface the same. i would need a way to alternate which of a,b,c are
-- incrementing per attempt. i remember reading something about alternating
-- streams in SICP but i have no idea how to do that in haskell.
-- i also want to refrain from "spoiling" the ItC, letting the book guide me,
-- so i won't search about this right now.

-- this is my first solution. by searching a bit i discovered there are better 
-- algorithms for this. i would implement if i was looking for performance but
-- this is good enough for now. maybe i'll revisit this later
pyCorrect :: Int -> Int -> Int -> (Int, Int, Int)
pyCorrect a b c
 | a * a + b * b == c * c = (a, b, c)
 | b >= c = pyCorrect 1 1 (c+1)
 | a >= b = pyCorrect 1 (a+1) c
 | otherwise = pyCorrect (a+1) b c

pyNext :: (Int, Int, Int) -> (Int, Int, Int)
pyNext (a,b,c) = pyCorrect a b (c+1)

pyNextRec :: Int -> (Int, Int, Int) -> (Int, Int, Int)
pyNextRec 0 (a,b,c) = pyCorrect a b c
pyNextRec i (a,b,c) = pyNextRec (i-1) (pyNext (pyCorrect a b c))

pythagoreanTriples2 :: [(Int, Int, Int)]
pythagoreanTriples2 = [ pyNextRec n (1,1,1) | n <- [0..] ]
