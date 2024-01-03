module Functor
  ( 
  ) where

{---------------}
{----- 101 -----}
{---------------}
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
  -- 🚨 Said differently: applies a function to the element inside the box
  -- fmap applies a function to the value while preserving its context
  --
  -- Two possible analogies for fmap:
  -- * A function that takes a function and a functor value and then maps that
  --   function over the functor value
  -- * A function that takes a function and lifts (see below) that function so
  --   it operates on functor values
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
