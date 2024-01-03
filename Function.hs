module Function
  ( 
  ) where

{---------------}
{----- 101 -----}
{---------------}
-- Simple function
double x = x * 2

-- Condition
doubleIfCondition x
  -- An if expression must return a value, not a statement
 =
  if x > 100
    then x
    else x * 2

-- We can also use let in list comprehension
-- We iterate over all the elements of the list, then we emit each mult item
-- with mult = a * b
letListComprehension :: [(Double, Double)] -> [Double]
letListComprehension xs = [mult | (a, b) <- xs, let mult = a * b]

-- Enforce signature type: Char to Char
withSignature :: [Char] -> [Char]
withSignature st = st

-- With multiple parameters: Int, Int to Int
withSignatureMultipleParameters :: Int -> Int -> Int
withSignatureMultipleParameters x y = x + y

{----------------------------}
{----- Pattern matching -----}
{----------------------------}
-- Note: a pattern has to be exhaustive
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- With a tuple
withTuple (_, 0) = error "denominator is zero"
withTuple (a, b) = a / b

-- With a list
tell :: (Show a) => [a] -> String -- Implement Show
tell [] = "empty"
tell (x:[]) = "one element"
tell (x:_) = "more than one element, the first is " ++ show x

-- As-pattern: allow you to break up an item according to a pattern, while still
-- keeping a reference to the entire original item
-- Note: (x:xs) is a list (e.g., xs matches y:z:[])
firstLetter all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

{------------------}
{----- Guards -----}
{------------------}
-- Guards: make a function to check if some property of the passed values is
-- true or false
guard x
  | x < 0 = "negative"
  | x == 0 = "zero"
  | otherwise = "positive"

-- We can use guards to implement a max function
max' a b
  | a < b = a
  | otherwise = b

{-----------------}
{----- Where -----}
{-----------------}
-- We can precompute a value with where
bmiTell weight height
  | bmi < skinny = "underweight"
  | bmi < normal = "good"
  | otherwise = "overweigth"
  where
    bmi = weight / height ^ 2
    skinny = 18
    normal = 25

-- Note: where is not only used with guards
whereEx = "hello " ++ foo
  where
    foo = "foo"

-- We can also use where bindings with pattern match
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

-- Note: Equivalent to
initials2 (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- A where can also be a function
whereFunc n = add n
  where
    add n = n + 1

-- Where with pattern matching
wherePattern ls = "The list is " ++ what ls
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."

wherePattern2 n = what n
  where
    what 0 = "zero"
    what 1 = "one"
    what v = "other"

{---------------}
{----- Let -----}
{---------------}
-- let is similar to where binding
-- Difference:
-- * Only let expressions are expressions, where bindings aren't
--   Note: if something is an expression, then it has a value
-- * Can't be used across guards
-- * let are defined before, where after
letEx n =
  let a = n + 1
      b = n + 2
   in a * b

{------------------------------}
{----- Recursion examples -----}
{------------------------------}
maximum' :: [Int] -> Int
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Replicate using pattern matching
replicate' :: Int -> a -> [a]
replicate' 0 n = []
replicate' 1 n = [n]
replicate' times n = n : replicate' (times - 1) n

-- Replicate using guards
replicate'' :: Int -> a -> [a]
replicate'' times n
  | times <= 0 = []
  | otherwise = n : replicate'' (times - 1) n

take' :: Int -> [a] -> [a]
take' count (x:xs)
  | count <= 0 = []
  | otherwise = x : take' (count - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x1:xs1) (x2:xs2) = (x1, x2) : zip' xs1 xs2

elem' :: (Eq a) => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs)
  | n == x = True
  | otherwise = elem' n xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

{----------------------------------}
{----- Higher-order functions -----}
{----------------------------------}
-- Each function takes only one parameter, it works using currying.
-- A curried function is a function that instead of taking several
-- parameters, always takes exactly one parameter. Then, when it's called
-- with that parameter, it returns a function that takes the next
-- parameter, and so on.
--
-- If we call a function with two few parameters, we get back a partially
-- applied function
multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z

partiallyAppliedFunc :: Int -> Int
partiallyAppliedFunc = multiThree 3 4

partiallyAppliedRes :: Int -> Int
partiallyAppliedRes z = partiallyAppliedFunc z

-- Infix functions (foo infixFunction bar; e.g., 3 + 2 => + is the infix
-- function)
-- Infix functions can be partially applied by using sections
-- To section an infix function, we can surround it with parentheses
divideByTen :: Float -> Float
divideByTen = (/ 10)

-- Another example
isUpper :: Char -> Bool
isUpper = (`elem` ['A' .. 'Z'])

-- Higher-order function: function can take functions as parameter
-- Example: Takes an (Int -> Int) function as parameter
higherOrderEx :: (Int -> Int) -> Int
higherOrderEx func = func 10

-- (+ 3) is a (Int -> Int) function
higherOrderRes = (+ 3) 10

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Note how (+) is passed for a (a -> b -> c) function
zipWithEx = zipWith' (+) [1, 2, 3] [4, 5, 6]

-- Flip the first two arguments
-- It takes a function with 2 parameters and returns a function with the
-- 2 parameters flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

flipEx = zipWith' (flip' div) [1, 2, 3] [10, 20, 30] -- [10,10,10]

{-----------------------------------------}
{----- Functional programmer toolbox -----}
{-----------------------------------------}
mapEx = map (+ 1) [1, 2, 3] -- [2,3,4]

filterEx = filter (> 10) [1, 2, 15, 20] -- [15,20]

filterWithListComprehension xs = [x | x <- xs, x > 10]

-- Largest number under 100,000 divisible by 3,829
-- Because we use head on the filtered list, it doesn't matter if the list is
-- finite or infinite, thanks to Haskell laziness, the evaluation stops when
-- the first solution is found
largestDivisible :: Integer
largestDivisible = head (filter p [99999,99999 ..])
  where
    p x = x `mod` 3829 == 0

-- takeWhile takes a predicate and a list, returns the list's elements as long
-- as the predicates is true
firstWord = takeWhile (/= ' ') "foo bar baz" -- foo

-- Fold
-- Folds allow to reduce a data structure to a single value
-- Use case: traverse a list to return a value
-- A fold takes a binary function (e.g., +), a starting value (accumulator), and
-- a list to fold up
sumList :: (Num a) => [a] -> a
sumList x = foldl (\acc a -> acc + a) 0 x

-- Equivalent to
sumList' x = foldl (+) 0 x

-- Fold from the right
-- Notice how in the lambda, the accumulator value comes last compared to foldl
sumList'' x = foldr (\a acc -> a + acc) 0 x

--
-- When we right fold over [1,2,3], we're essentially doing this:
-- f 1 (f 2 (f 3 acc))
--
-- Main difference between left and right folds: right folds work on infinite
-- lists, whereas left ones don't
--
-- Note: The ++ function is much slower than :, so we usually use right folds
-- when we’re building up new lists from a list.
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs =
  foldl
    (\acc a ->
       if a == x
         then True
         else acc)
    False
    xs

-- foldl1 and foldr1 work much like foldl and foldr, except that we don't have
-- to provide the starting accumulator
-- They assume the first element of the list to be the starting accumulator
-- Warning: rumtime error is empty list
sumListWithFoldl1 :: (Num a) => [a] -> a
sumListWithFoldl1 xs = foldl1 (+) xs

-- Some fold example
foldReverse :: [a] -> [a]
foldReverse xs = foldl (\acc x -> x : acc) xs []

foldProduct :: (Num a) => [a] -> a
foldProduct xs = foldl1 (*) xs

foldFilter :: (a -> Bool) -> [a] -> [a]
-- 🚨 This version is the same as the line below where we omit xs
-- It works because of partial application
-- In Haskell, functions are curried by default, which means that a function
-- that takes multiple arguments can be partially applied to fewer arguments,
-- creating a new function
foldFilter f =
  foldr
    (\x acc ->
       if f x
         then x : acc
         else acc)
    []

-- foldFilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs
-- Note: This foldl version doesn't work as the left element of : needs to be a
-- single element
--foldFilter f = foldl (\acc x -> if f x then acc : x else acc) []
foldLast :: [a] -> a
foldLast xs = foldl1 (\_ x -> x) xs

-- scanl and scanr are like foldl and foldr except they report all the acc
-- values in the form of a list
sumWithIntermediates :: (Num a) => [a] -> [a]
sumWithIntermediates xs = scanl (+) 0 xs

sumWithIntermediatesEx = sumWithIntermediates [1, 2, 3] -- [0,1,3,6]

-- Another example, how many elements does it take for the sum of the square
-- roots of all natural numbers to exceed 1,000
sqrtSum :: Int
sqrtSum = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

{-------------------}
{----- Lambdas -----}
{-------------------}
-- A lambda is an anonymous function that we use when we need a function only
-- once; usually, passing it to a higher-order function
lambdaEx = map (\x -> x + 10) [1 .. 3] -- [11,12,13]

-- Equivalent to the use of a partially applied function
partiallyAppliedEx = map (+ 10) [1 .. 3] -- [11,12,13]

-- A lambda can take any number of parameters
lambdaMultipleParam = zipWith (\a b -> a + b) [1 .. 3] [10, 20, 30] -- [11,22,33]

-- Lambda with pattern matching
lambdaPatternEx = map (\(a, b) -> a + b) [(1, 10), (2, 20), (3, 30)] -- [11,22,33]

-- Flip arguments example using a lambda
-- Note: When we write a lambda without parentheses, Haskell assumes that
-- everything to the right of -> belongs to it
flipWithLambda :: (a -> b -> c) -> b -> a -> c
flipWithLambda f = \x y -> f y x

flipWithLambdaEx = flipWithLambda (div) 5 10 -- 2

{--------------------------------}
{----- Function application -----}
{--------------------------------}
-- The $ function is called the function application operator
-- Definition:
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- Syntactic sugar to replace parenthesis
-- $ is the (, whereas ) is the right side of the expression
withoutFunctionApp = sum (filter (> 10) (map (* 2) [2 .. 10]))

withFunctionApp = sum $ filter (> 10) $ map (* 2) [2 .. 10]

-- $ is right associative meaning f $ g $ x <=> f $ (g $ x)
-- $ is just another function
-- Here, every function in the list is applied to 3
functionAppEx = map ($ 3) [(4 +), (10 *), (3 -)] -- [7,30,0]

{--------------------------------}
{----- Function composition -----}
{--------------------------------}
-- Function composition is defined like this: (f º g)(x) = f(g(x)).
-- We can use function composition with the . function
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
--
-- f (g (z x)) <=> f . g . z
-- negate . sum is a function that takes a list, applies sum, and then negate
compositionEx = map (negate . sum) [[1 .. 5], [10, 13]] -- [-15,-23]

-- Applies max, cos, tan, and negate
compositionEx2 :: Float -> Float
compositionEx2 = negate . tan . cos . max 50

withoutComposition = sum (replicate 5 (max 6 7)) -- 35

withComposition = sum . replicate 5 $ max 6 7 -- 35
