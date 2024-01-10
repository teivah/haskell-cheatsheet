module AdvancedTypes
  ( 
  ) where

import Control.Applicative as A
import Control.Monad (guard)
import qualified Data.Traversable

{--------------------}
{----- 🚨 TL;DR -----}
{--------------------}
{---- Functor -----}
-- * Functor: an abstractions that allows for mapping a function over values in a
--   context (box), without altering the context itself
-- * fmap: takes a function and a functor value, applies the function and
--   applies that function over the functor value
--
--   (a -> b)   ->   f a -> f b
--   --------        ---    ---
--   Function      Functor Functor
-- value to value
{---- Applicative -----}
-- * Applicative: an abstraction that allows for applying functions wrapped in
--   a context to values in the same context
--   Difference with functor: higher-level abstraction that allows to apply
--   functions to multiple wrapped values in a context
-- * <$>: fmap version for applicative
--   It puts something inside a box
--   Can be used on functions as an input to <*>
--   Works with a left and right operand
--
--   (Functor f)   =>   (a -> b)   ->   f a -> f b
--   -----------        --------        ---    ---
-- With f a functor     Function      Functor Functor
--                   value to value
--
-- * <*>: takes a functor value that has a function in it and another functor,
--   and extracts that function from the first functor and then maps it over
--   the second one
--   Works with a left and right operand
--
--   f (a -> b)   ->   f a -> f b
--   ----------        ---    ---
-- Functor function  Functor Functor
--  value to value
--
{----- Monad -----}
-- * Monad: an extension of applicative that provides a way to sequence and
--   compose then while preserving the contexts
-- * >>= (bind):
--
--   (Monad m)  =>  m a -> (a -> m b)  ->  m b
--   ---------      ---    ----------      ---
-- With m a monad  Monad    Function      Monad
--                       value to Monad
--
-- * return: wraps a value inside a monad
--
--   return :: a -> m a
--             -    ---
--           Value Monad
--
-- * do: chain monadic expressions
--
tldr = ()

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
  -- 🚨 Said differently: applies a function to the element inside the box
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
-- Use case: combine different computations (I/O, combinations that might have
-- failed, etc.)
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
  -- 🚨 First argument is at the left of <*>: Just f, the second arg is a
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

-- ZipList implementation of Applicative
-- If we don't need combinations but only applying the first function to the
-- first value, the second function to the second value, etc. we need the
-- ZipList applicative
instance Applicative' ZipList
  -- Note how pure produces a infinite list
                                            where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- IO implementation of Applicative
instance Applicative' IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

applicativeEx = do
  -- <*> takes a Just functor with the function (+3) insides and the Just
  -- functor 9
  let _ = Just (+ 3) A.<*> Just 9 -- Just 12
  let _ = A.pure (+ 3) A.<*> Just 9 -- Just 12
  -- We can even chain calls
  let _ = A.pure (+) A.<*> Just 3 A.<*> Nothing -- Nothing
  -- With lists
  let _ = [(+ 1), (* 2)] A.<*> [1, 2, 3] -- [2,3,4,2,4,6]
  let _ = [(+), (*)] A.<*> [1, 2] A.<*> [3, 4] -- [4,5,5,6,3,4,6,8]
  ()

-- We can also use <$> which is an infix synonym for fmap
-- Definition:
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x
-- 🚨 Same as <*>: left and right operand
infixEx = do
  -- Apply +1 on a value inside a box
  let _ = (+ 1) A.<$> (Just 5) -- Just 6
  -- 🚨 Partial application, we moved (2 +) inside a box
  let _ = (\a b -> a + b) A.<$> (Just 2) -- Just (2 +)
  -- In conjunction with <*>
  let _ = (\a b -> a + b) A.<$> (Just 2) A.<*> (Just 3) -- Just 5
  -- Another example to make things clear
  -- We move the Int -> Int function inside a box [Int -> Int]
  let a = (\a b -> a + b) A.<$> [1] -- [Int -> Int]
  -- In conjunction with <*>
  let b = (\a b -> a + b) A.<$> [1] A.<*> [1, 2, 3] -- [2,3,4]
  b

-- Benefits of applicative over functors: allow to apply function(s) to multiple
-- wrapped values in a context
benefitsApplicative = do
  -- Using functor
  -- In this case, result is a Just (2 +), we can't apply it directly to maybeY
  -- Note fmap would work if add was an Int -> Int function
  -- Here add is an Int -> Int -> Int function
  let a = fmap add maybeX -- Just (2 +)
  -- The following line doesn't compile as fmap would accept a (2 +) function,
  -- not Just (2 +)
  -- let b = a maybeY
  --
  -- Using applicative
  let result = add A.<$> maybeX A.<*> maybeY -- Just 5
  --           ----------------
  -- Transforms add into a function inside the Maybe context
  --          Maybe (Int -> Int)
  --                            -----
  --                 Applies the function to maybeY
  ()
  where
    add x y = x + y
    maybeX = Just 2
    maybeY = Just 3

-- liftA2: applies a function between two applicatives
-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2Ex = do
  let _ = A.liftA2 (:) (Just 3) (Just [4]) -- Just [3,4]
  -- Same as
  let _ = (:) A.<$> Just 3 A.<*> Just [4] -- Just [3,4]
  ()

-- sequenceA: takes a list of applicative values and returns an applicative
-- values that has a list as its result value
sequenceAEx = do
  let _ = sequenceA [Just 1, Just 2] -- Just [1,2]
  -- Useful when we have a list of functions and we want to feed the same input
  -- to all of them and then view the list of results
  -- For example, we have a number and we're wondering if it satisfies a list of
  -- predicates
  -- Without sequenceA:
  let a = map (\f -> f 7) [(> 4), (< 10), odd] -- [True,True,True]
  let b = and $ a -- True
  -- With sequenceA
  let a = sequenceA [(> 4), (< 10), odd] 7 -- [True,True,True]
  let b = and $ a -- True
  ()

-- Applicative laws
-- Most important law:
-- pure f <*> x = fmap f x
law = ()

{------------------}
{----- Monads -----}
{------------------}
-- An extension of applicative
-- Provide a solution to the following problem: how do we apply a function of
-- type `a -> m b` to a value of type `m a`
-- Monad examples: Maybe, list
--
-- Monad functions:
-- * (>>=), called bind:
--   (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--   Example:
--   (>>=) :: (Monad Either) => Just Int -> (Int -> Just String) -> Just String
--   Allows to preserve the context of the value to which the function applies
-- * return: wraps a value inside a Monad
-- * do: chain monadic expressions
-- * (<-): extract a value from a Monad
-- * (<=<), called fish
--   Used for function composition (similar to .)
--
-- Monad class:
--  class Monad m where
--    return :: a -> m a
--    (>>=) :: m a -> (a -> m b) -> m b
--    (>>) :: m a -> m b -> m b
--    x >> y = x >>= \_ -> y
--    fail :: String -> m a
--    fail msg = error msg
monadEx = Just 9 >>= \x -> return (x + 1) -- Just 10

-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
monadEx2 = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

-- do is another way to chain monadic values
--
-- Do expressions are written line by line; they may look as imperative code but
-- they're just sequential, as each value in each line relies on the result of
-- the previous ones, along with their contexts
--
-- Note: be it <- or let, the variable is assigned a non-monadic value
-- Yet, <- takes a monadic value
withDo :: Maybe String
withDo = do
  x <- Just 3 -- x is 3
  y <- Just "!"
  Just (show x ++ y) -- Just "3!"

-- The same using (>>=)
withoutDo :: Maybe String
withoutDo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) -- Just "3!"

-- If any of the values that we try to extract from are Nothing, the whole
-- expression will result in a Nothing
withDoNothing = do
  x <- Just 3 -- x is 3
  y <- Just "!"
  Nothing
  Just (show x ++ y) -- Just "3!"

-- 🚨 In case of an array, the elements are bound to the `x` variable element by
-- element
withDoArray = do
  x <- foo 1 -- x is first bound to 1 and then 2
  foo x -- [1,2,2,3]

foo :: Int -> [Int]
foo a = [a, a + 1]

-- Return a Maybe Char from the first letter of a string using pattern matching
monadPatternMatching :: String -> Maybe Char
monadPatternMatching str = do
  -- Failed pattern matching (e.g., "") returns a Nothing (fail function)
  (x:xs) <- Just str
  return x

-- Applicative functors don't allow for the applicative values to interact with
-- each other very much; at best, they can be used as parameters to a function
-- by using the applicative style
benefitsMonad = ()

validateAge :: Int -> Maybe Int
validateAge age =
  if age >= 18
    then Just age
    else Nothing

greet :: String -> String
greet name = "Hello " ++ name

-- Does not compile:
-- withoutMonad age name = greet A.<$> (Just name) A.<*> validateAge age
--                                      ---------        ---------------
--                                     Maybe String         Maybe Int
--                         ------------------------
--                          String -> Maybe String
--                     Should be a Maybe (Int -> String)
withMonad :: Int -> String -> Maybe String
withMonad age name = do
  validAge <- validateAge age
  return $ greet name

withMonad' :: Int -> String -> Maybe String
withMonad' age name = validateAge age >>= \_ -> return $ greet name

--                    -----------         ---------------------------
--                     Maybe Int                Lambda function
--                                            Int -> Maybe String
--
del = ()

-- Monad laws
--
-- * First law: left identity
--   If we take a value, put it in a default context with return and then feed
--   it to a function using >>=, that's the same as just taking the value and
--   applying the function to it
--   return x >>= f   is equivalent to   f x
--
--   return 3 >>= (\x -> Just (x+100000))
--   (\x -> Just (x+100000)) 3
--   Just 100003
--
-- * Second law: right identity
--   If we have a monadic value and we use >>= to feed it to return, the result
--   is our original monadic value
--   m >>= return   is equivalent to   just m
--
-- * Third law: associativity
--   When we have a chain of monadic function applications with >>=, it
--   shouldn’t matter how they’re nested
--   (m >>= f) >>= g   is equivalent to   m >>= (\x -> f x >>= g)
--
{---------------------}
{----- MonadPlus -----}
{---------------------}
-- The MonadPlus type class if for monads that can also act as monoids
--
-- Definition:
-- class Monad m => MonadPlus m where
--    mzero :: m a
--    mplus :: m a -> m a -> m a
--
-- * mzero is the mempty synonym
-- * mplus is the mappend synonym
--
-- [] implementation
-- instance MonadPlus [] where
--    mzero = []
--    mplus = (++)
monadPlus = ()

-- guard function: if the guard succeeds, the result is an empty tuple
-- guard :: (MonadPlus m) => Bool -> m ()
--   guard True = return ()
--   guard False = mzero
guardEx = do
  let _ = guard (4 > 2) :: Maybe () -- Just ()
  let _ = guard (2 > 4) :: Maybe () -- Nothing
  ()

-- When to use guard? As a filter for monadic values
guardBenefits = do
  let _ = guard (4 > 2) >> return "cool" :: [String] -- ["cool"]
  let _ = guard (2 > 4) >> return "cool" :: [String] -- []
  ()

-- Note: the following is the same as
-- [ x | x <- [1..50], '7' `elem` show x ]
-- So, filtering in list comprehensions is the same as using guard
guardBenefits' = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x -- [7,17,27,37,47]
