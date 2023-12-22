-- TODO
-- foo` denotes either a strict version of a function (i.e., not lazy) or a slightly modified version of a function or variable with a similar name
main :: IO ()
main = do
  -- Variable
  let a = 42
  print a
  -- Function call
  let a = double 5
  -- The same function works with floats
  let a = double 5.2
  -- Condition
  let a = doubleIfCondition 10
  -- Definition
  let a = def
  -- End
  print ""

double x = x * 2

doubleIfCondition x
  -- An if expression must return a value, not a statement
 =
  if x > 100
    then x
    else x * 2

-- As def doesn't take a parameter, it's not a function
-- It's a definition or a name
def = "foo"
