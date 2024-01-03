module Types
  ( 
  ) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

{---------------}
{----- 101 -----}
{---------------}
-- id takes a parameter and return the same thing
idEx :: Int -> Int
idEx f = id f

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
--
-- class Eq a where
-- (==) :: (Eq a) => a -> a -> Bool
--
-- In this example, we take a generic type a with the constraint that it
-- implements Eq
eqEx :: (Eq a) => a -> a -> Bool
eqEx x y =
  if x == y
    then True
    else False

-- Eq type class definition:
-- `class Eq a where` means a new type class called Eq
--
-- class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)
-- Note: It's not mandatory to implement the function bodies (stay abstract?)
--
-- Deriving type class by hand (i.e., implementing a type class)
data TrafficLight
  = Red
  | Yellow
  | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- Another example is the Show type class
showEx = show 3 -- "3"

-- Read type class, can be used for conversions, for example
readEx :: String -> Int
readEx s = read s :: Int

{-----------------}
{----- Maybe -----}
{-----------------}
-- Maybe can have either zero or just one element
maybeEx :: Maybe Int
maybeEx = find (> 4) [1 .. 10] -- Just 5

maybeGet :: Int
maybeGet =
  case maybeEx of
    Just n -> n
    Nothing -> -1

maybeGet' :: Int
maybeGet' = fromMaybe 0 m -- 5
  where
    m = Just 5

{---------------}
{----- Map -----}
{---------------}
-- Map example
mapDSEx = do
  -- Create
  let m = Map.fromList [(1, "one"), (2, "two"), (3, "three")]
  -- Lookup
  let v = Map.lookup 1 m -- Maybe string
  -- Insert (note how the map is immutable)
  let m2 = Map.insert 10 "ten" m
  -- When a duplicate is found, do an action
  let m = Map.fromListWith (+) [(1, 1), (2, 3), (2, 4)] -- fromList [(1,1)(2,7)]
  m

-- Set example
setDSEx = do
  let m = Set.fromList [1, 2, 3]
  0

-- Custom data types
-- Enum
data Direction
  = Up
  | Down
  | Left
  | Right

-- Create a custom data type
-- Value constructor: a function that returns a value of a data type
-- A shape is either a circle or a rectangle
data Shape
  = Circle Float Float Float
  | Rectangle Float Float Float Float
  deriving (Show)

-- Instantiate a circle
constructorEx = Circle 1 2 3

-- Receive a shape using pattern matching
shapeEx :: Shape -> Float
shapeEx (Circle _ _ v) = v
shapeEx (Rectangle _ _ _ v) = v

-- To export Shape:
--
-- modules Foo
-- ( Shape(..) )
--
-- By using `Shape(..)`, we export all the value constructor for Shape
-- Or, we could use `Shape`, to export only the class type (in this case we
-- also have to export a custom constructor just like `Map.fromList`)
-- Benefit: the structure can change without breaking existing programs
--
--
-- We can also use record syntax to create a handier version
-- Note how Person is an instance of the Show and Eq type classes
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Show, Eq)

-- Instantiate (more readable, note: we need all the parameters)
recordSyntaxEx = Person {firstName = "Bill", lastName = "Gates", age = 30}

data DataEx
  = Foo
  | Bar
  | Baz
  deriving (Eq, Ord, Bounded, Enum)

-- Thanks to Ord type class, we can compare values
ordEx = Foo < Bar

-- Thanks to Bounded type class, we can get the min or max value
boundedEx = (minBound :: DataEx, maxBound :: DataEx) -- (Foo,Baz)

-- Thanks to Enum type class, we can get the predecessor or successor
enumEx = (pred Bar, succ Bar) -- (Foo,Baz)

-- Type parameters
-- Type constructors can take types as parameters to produce new types
-- Example:
-- data Maybe a = Nothing | Just a
-- a is the type parameter, Maybe is a type constructor
-- Note: a concrete type is a type that doesn't take any type parameters
-- (values can have only types that are concrete types)
--
--
-- Type parameter is inferred, but we can make it explicit
typeParameterEx = Just 3 :: Maybe Int

-- Type synonyms
--
-- A type synonym is a type that can be used interchangeably
-- type String = [Char]
--
-- We can use type synonyms to make a signature easier to ead
type PhoneNumber = String

type Name = String

registerPhone :: PhoneNumber -> Name -> Bool
registerPhone _ _ = True

-- Type synonyms can be parameterized
type AssocList k v = [(k, v)]

-- To instantiate an AssocList
assocListEx = [(1, 2), (3, 4), (5, 6)] :: AssocList Int Int

-- A type synonym can also be partially applied
type IntMap v = Map.Map Int v

-- Is equivalent to
type IntMap' = Map.Map Int

-- Either is either left or right
data Either' a b
  = Left' a
  | Right' b
  deriving (Eq, Ord, Read, Show)

-- Recursive data structures
--
-- List example
-- Note: Cons is another word for `:`
data List' a
  = Empty'
  | Cons a (List' a)
  deriving (Show, Read, Eq, Ord)

-- Tree example
data Tree' a
  = EmptyTree'
  | Node' a (Tree' a) (Tree' a) -- Left and right
  deriving (Show)

singleton' :: a -> Tree' a
singleton' x = Node' x EmptyTree' EmptyTree'

treeInsert :: (Ord a) => a -> Tree' a -> Tree' a
treeInsert x EmptyTree' = singleton' x
treeInsert x (Node' a left right)
  | x == a = Node' x left right
  | x < a = Node' a (treeInsert x left) right
  | x > a = Node' a left (treeInsert x right)