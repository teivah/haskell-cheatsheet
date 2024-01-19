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

-- Referential transparency: key concept in FP, refers to an aspect of the
-- behavior of pure functions
-- * Substitutability: an expression is said to be referentially transparent if
--   it can be replaced with its corresponding value without changing the
--   program's behavior
-- * Predictability and reasoning: makes it easier to reason about the behavior
--   of a program
-- * No side effects: implies that functions do not have side effects
--
-- See:
-- Functions.hs
-- Lists.hs
-- IO.hs
-- Types.hs
-- AdvancedTypes.hs
-- Others.hs
--
-- Main definition (usually, we don't specify a type declaration for main)
main = do
  print "Hello, World!"
