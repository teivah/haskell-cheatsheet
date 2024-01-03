module Functors
  ( applicativeEx
  ) where

import Control.Applicative as A

{--------------------}
{----- 🚨 TL;DR -----}
{--------------------}
-- * Functor: any data type that allows for mapping a function over values in a
--   context (box), without altering the context itself
-- * fmap: takes a function and a functor value, applies the function and
--   applies that function over the functor value
--
--   (a -> b) -> f a -> f b
--   --------    ---    ---
--   Function  Functor Functor
--
-- * Applicative: any data type that allows for applying functions wrapped in
--   a context to values in the same context
-- * <*>: takes a functor value that has a function in it and another functor,
--   and extracts that function from the first functor and then maps it over
--   the second one
--
--   f (a -> b)    ->    f a -> f b
--   ----------          ---    ---
--   Functor function  Functor Functor
del = ()

{-------------------}
{----- Functor -----}
{-------------------}
-- A functor is a type class that abstracts the concept of mapping a function
-- over elements
-- For example: list, Maybe, Either, IO, etc. are instances of the functor type
-- class
-- Shorter version: list, Maybe, Either, IO, etc. are functors
--
-- Note that f is not a concrete type but a type constructor that takes one
-- type parameter (e.g., Maybe Int is concrete but Maybe is a type
-- constructor)
class Functor' f where
  -- Takes a function that maps a `a` into a `b` and a `a` box, returns a `b`
  -- box
  -- 💡 Said differently: applies a function to the element inside the box
  -- fmap applies a function to the value while preserving its context
  --
  -- Two possible analogies for fmap:
  -- * A function that takes a function and a functor value and then applies
  --   that function over the functor value
  -- * A function that takes a function and lifts (see below) that function so
  --   it operates on functor values
  -- See `res/fmap.png`
  fmap' :: (a -> b) -> f a -> f b

-- Map is just an fmap that works only with lists:
-- instance Functor [] where
--   fmap = map
-- Note: If we bind the result of an I/O action to a name, only to apply a
-- function to that and call something else, we should consider using fmap
fmapEx = fmap (++ ", World!") (Just "Hello")

{------------------------}
{----- Functor laws -----}
{------------------------}
-- Law 1: if we map the id function over a functor value, the functor value that
-- we get back should be the same as the original value
-- Formally: fmap id = id
law1Ex = fmap id (Just 3) -- Just 3

-- Law 2: composing two functions and then mapping the resulting function over a
-- functor should be the same as first mapping one function over the functor and
-- then mapping the other one
-- Formally: fmap (f . g) = fmap f . fmap g
law2Ex = do
  let a = fmap ((+ 1) . (* 2)) [1, 2, 3]
  let b = fmap (+ 1) . fmap (* 2) $ [1, 2, 3]
  a == b -- True

{----------------------------}
{----- Function lifting -----}
{----------------------------}
-- Function lifting is the process of transforming a function that operates on
-- simple values into a function that operates on more complex structures like
-- functors
-- Regular function
addOne :: Int -> Int
addOne x = x + 1

-- Lifted functions using fmap
addOneF :: Functor f => f Int -> f Int
-- addOneF is a function that takes a functor over a number and returns a
-- functor over a number
addOneF = fmap (+ 1)

{-----------------------}
{----- Applicative -----}
{-----------------------}
-- Applicative is the following type class (no default implementation)
class (Functor f) =>
      Applicative' f
  where
  -- Take a value of any type and return an applicative value with that value
  -- inside (analogy: inside of a box)
  pure :: a -> f a
  -- Some similarities with fmap:
  -- * fmap takes a function and a functor value and applies the function
  --   inside the functor value
  -- * <*> (reads as "applies") takes a functor value that has a function in it
  --   and another functor, and extracts that function from the first functor
  --   and then maps it over the second one
  -- See `res/<*>.png`
  (<*>) :: f (a -> b) -> f a -> f b

-- Maybe implementation of Applicative
instance Applicative' Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  -- Note: something in this example is a Maybe f value
  -- Applies the function inside Just to something
  -- 💡 First argument is at the left of <*>: Just f, the second arg is a
  -- Note: This is a convention followed in the context of the Applicative type
  -- class
  (Just f) <*> a = fmap f a

-- List implementation of Applicative
instance Applicative' [] where
  pure x = [x]
  -- fs is a functor value with a function, xs is a functor value
  -- It applies every possible function from the left list to every possible
  -- value from the right list
  -- The result being every possible combination of applying the functions to
  -- the values
  fs <*> xs = [f x | f <- fs, x <- xs]

applicativeEx = do
  -- <*> takes a Just functor with the function (+3) insides and the Just
  -- functor 9
  let a = Just (+ 3) A.<*> Just 9 -- Just 12
  let b = A.pure (+ 3) A.<*> Just 9 -- Just 12
  -- We can even chain calls
  let c = A.pure (+) A.<*> Just 3 A.<*> Nothing -- Nothing
  -- With lists
  let d = [(+ 1), (* 2)] A.<*> [1, 2, 3] -- [2,3,4,2,4,6]
  let e = [(+), (*)] A.<*> [1, 2] A.<*> [3, 4] -- [4,5,5,6,3,4,6,8]
  -- We can also use <$> which is an infix synonym for fmap
  -- Definition:
  -- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
  -- f <$> x = fmap f x
  let f = (++) A.<$> Just "foo" A.<*> Just "bar" -- Just "foobar"
  ()
