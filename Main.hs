main :: IO ()
main = do
  -- Variable
  let a = 42
  print a

  -- Function call
  let a = double 5
  -- The same function works with floats
  let a = double 5.2

  -- Conditions
  let a = doubleIfCondition 10

  print ""

double x = x * 2

doubleIfCondition x = if x > 100 then x else x * 2
