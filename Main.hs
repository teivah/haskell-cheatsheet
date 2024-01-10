-- Module
-- See Geometry/Cube.hs
-- Import all the elements of the Geometry.Cube module
-- Using `qualified` means we can access the element with the Cube prefix
-- E.g., Cube.area
import qualified Geometry.Cube

import AdvancedTypes
import qualified Control.Applicative
import Data.List
import Debug.Trace
import Functions
import IO
import Lists
import Types

-- To exclude an element from import, we can use hiding
-- Note that `as S` allows to create an alias
import qualified Geometry.Sphere as S hiding (volume)

-- TODO
-- foo` denotes either a strict version of a function (i.e., not lazy) or a slightly modified version of a function or variable with a similar name
--
-- TODO
--
-- Tracing
tracingEx = trace (show (foo)) $ foo
  where
    foo = 42

-- Tuples
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

matching = 0

-- Functions.hs
-- Lists.hs
-- IO.hs
-- Types.hs
-- AdvancedTypes.hs
--
-- Usually, we don't specify a type declaration for main
main = do
  print "Hello, World!"
