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

-- List concatenation (++ operator -> list1 ++ list2)
-- A concatenation copies both lists (it has to iterate over the entire first
-- list), so it can take a while
concatenatedList = [1, 2, 3] ++ [4, 5, 6]

-- Adding an element to the beginning (: operator -> elem : list)
-- Compared to concatenations, adding an element is instantaneous
addingHeadToList = 1 : [2, 3]

accessingListElement x = [1, 2, 3] !! x

innerList = [[1, 2, 3], [4, 5, 6]]

{- String -}
-- A string is a list of characters
string = "hello"

-- Same as
listOfChars = ['h', 'e', 'l', 'l', 'o']

-- Therefore, we can use the same list operation
stringList = 'x' : ['y', 'z']

main :: IO ()
main = do
  let v = var
  print v
  print ""
