-- TODO
-- foo` denotes either a strict version of a function (i.e., not lazy) or a slightly modified version of a function or variable with a similar name
--
--
{- Variables -}
-- Variable
var = 42

-- let introduces a local binding
-- in specifies the expression that represents the value of the let block
var2 =
  let foo = 42
   in foo

{- Functions -}
-- Function
double x = x * 2

-- Condition
doubleIfCondition x
  -- An if expression must return a value, not a statement
 =
  if x > 100
    then x
    else x * 2

{- List -}
-- A list is a homogeneous data structure
-- (i.e., it stores elements of the same type)
list = [1, 2, 3]

-- A list is syntactic sugar for
representation = 1 : 2 : 3 : [] -- [1,2,3]

-- List concatenation (++ operator -> list1 ++ list2)
-- A concatenation copies both lists (it has to iterate over the entire first
-- list), so it can take a while
concatenatedList = [1, 2, 3] ++ [4, 5, 6] -- [1, 2, 3, 4, 5, 6]

-- Adding an element to the beginning (: operator -> elem : list)
-- Compared to concatenations, adding an element is instantaneous
addingHeadToList = 1 : [2, 3] -- [1, 2, 3]

accessingListElement x = [1, 2, 3] !! x

innerList = [[1, 2, 3], [4, 5, 6]]

listComparison1 = [1, 2, 3] < [4, 5, 6] -- True

-- A nonempty list is greater than an empty one
listComparison2 = [1, 2, 3] > [] -- True

-- Return head
listHead = head [1, 2, 3] -- 1

--Delete first element
listTail = tail [2, 3] --  [2,3]

-- Delete last element
listInit = init [1, 2, 3] -- [1,2]

-- Get length
listLength = length [1, 2, 3] -- 3

-- Check if list is empty
listNull = null [] -- True

-- Reverse a list
listReverse = reverse [1, 2, 3] -- [3,2,1]

-- Take the first n elements
listTake = take 3 [1, 2, 3, 4, 5] -- [1,2,3]

-- Drop the first n elements
listDrop = drop 3 [1, 2, 3, 4, 5] -- [4,5]

-- Get the maximum
listMaximum = maximum [1, 2, 3] -- 3

-- Get the minimum
listMinimum = minimum [1, 2, 3] -- 1

-- Sum of the elements in a list
listSum = sum [1, 2, 3] -- 6

-- Product of the elements in a list
listProduct = product [1, 2, 3] -- 6

-- Check if an element is in a list
listCheck = 3 `elem` [1, 2, 3] -- True

-- From elem 1 to 20 (included)
listRange = [1 .. 20]

-- From 2 to 10 every 2 elements
listRangeWithStep = [2,4 .. 10] -- [2,4,6,8,10]

-- Infinite list
listInfinite = [1,2 ..] -- [1,2,3,4,5,...]

-- Infinite cycle
listCycle = cycle [1, 2, 3] -- [1,2,3,1,2,3,1,2,3,...]

-- Repeat
listRepeat = repeat 5 -- [5,5,5,5,5,...]

-- Repeat the same element a certain number of times
listReplicate = replicate 3 5 -- [5,5,5]

{- List comprehensions -}
{- A way to filter, transform, and combine lists -}
-- Take all numbers less than or equal to 5, multiply each one by 2
-- We bind each elements  from [1..5] to x
listComprehension = [x * 2 | x <- [1 .. 5]] -- [2,4,6,8,10]

-- We can also add a predicate
listComprehensionWithFiltering = [x * 2 | x <- [1 .. 5], x * 2 >= 8] -- [8,10]

-- Replaces every odd number greater than 10 with bar, and add every odd number less than 10 with foo
listComprehensionWithFiltering2 xs =
  [ if x < 10
    then "foo"
    else "bar"
  | x <- xs -- Iterate over each element
  , odd x -- Only for odd elements (predicate 1)
  , x > 1 -- And x greater than 1 (predicate 2)
  ]

-- Takes the combinations of the two lists
listComprehensionFromMultipleLists = [x + y | x <- [1, 2], y <- [10, 20]] -- [11,21,12,22]

-- Length of a list using list comprehension
-- We use _ as we don't care to map each element to a particular var
length' xs = sum [1 | _ <- xs] -- Sum of [1,1,1] -> 3

-- Remove all the odd numbers in an inner list
removeOdds xxs = [[x | x <- xs, even x] | xs <- xxs]

{- String -}
-- A string is a list of characters
string = "hello"

-- Same as
listOfChars = ['h', 'e', 'l', 'l', 'o']

-- Therefore, we can use the same list operation
stringList = 'x' : ['y', 'z']

-- List comprehension on string
removeUppercase st = [c | c <- st, c `elem` ['a' .. 'z']]

{- Tuples -}
-- Store heterogeneous elements as a single value
-- A tuple is of a fixed size, we can't add elements
tuple = (1, "a", 3.2)

-- Pair (tuple of two)
pair = (1, 2)

-- Access the first element (pair only)
pairFirstElement = fst (1, 2)

-- Access the second element (pair only)
pairSecondElement = snd (1, 2)

-- Triple (tuple of three)
triple = (1, 2, 3)

-- Produce a list of pairs based on the matching elements in a list
zipLists = zip [1, 2, 3] ["a", "b", "c", "d"] -- [(1,"a"),(2,"b"),(3,"c")] -- Note that "c" and "d" are ignored

{- Types -}
-- Enforce signature type: Char to Char
withSignature :: [Char] -> [Char]
withSignature st = st

-- With multiple parameters: Int, Int to Int
withSignatureMultipleParameters :: Int -> Int -> Int
withSignatureMultipleParameters x y = x + y

-- Common types:
-- Int: integer, bounded
-- Integer: large integers, unbounded
-- Float: floating-point
-- Double: floating-point with double the precision
-- Bool: boolean
-- Char: Unicode character
-- String: just another name for [Char]
-- Tuple: tuple
--
-- Type variable: generic type
-- Functions that use type variables are called polymorphic functions
-- Example: head
-- Usually, type variables have names with a single character; for example:
generic :: [a] -> a
generic a = head a

-- Type class is an *interface* that defines some behavior
-- If a type is an instance of type class, then it supports and implements the
-- behavior the type class describes
-- For example, == is a type class:
-- (==) :: (Eq a) => a -> a -> Bool
-- => is the class constraint symbol
-- Another example is the Show type class
showEx = show 3 -- "3"

-- Read type class, can be used for conversions, for example
readEx :: String -> Int
readEx s = read s :: Int

{- Pattern matching -}
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
firstLetter all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

{- Guards -}
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
whereExample = "hello " ++ foo
  where
    foo = "foo"

-- We can also use where bindings with pattern match
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

{- Enum -}
data Direction
  = North
  | South
  | East
  | West

main :: IO ()
main = do
  let v = var
  print v
  let v = initials "foo" "bar"
  print v
  print ""
