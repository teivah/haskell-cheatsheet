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

-- List concatenation
concatenatedList = [1, 2, 3] ++ [4, 5, 6]

{- String -}
-- A string is a list of characters
string = "hello"

-- Same as
listOfChars = ['h', 'e', 'l', 'l', 'o']

main :: IO ()
main = do
  let v = var
  print v
  print ""
