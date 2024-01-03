module Lists
  ( 
  ) where

{---------------}
{----- 101 -----}
{---------------}
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

{-------------------------------}
{----- List comprehensions -----}
{-------------------------------}
-- A way to filter, transform, and combine lists
-- Take all numbers less than or equal to 5, multiply each one by 2
--
-- Said differently: we bind each elements from [1..5] to x and we emit all the
-- x * 2 items
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

{------------------}
{----- String -----}
{------------------}
-- A string is a list of characters
string = "hello"

-- Same as
listOfChars = ['h', 'e', 'l', 'l', 'o']

-- Therefore, we can use the same list operation
stringList = 'x' : ['y', 'z']

-- List comprehension on string
removeUppercase st = [c | c <- st, c `elem` ['a' .. 'z']]

-- Get the words in a string
wordsEx = words "foo bar baz" -- ["foo","bar","baz"]
