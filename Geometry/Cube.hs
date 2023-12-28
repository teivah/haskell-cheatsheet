-- A module is a file that defines some functions, types, and type classes
-- A Haskell program is a collection of modules
-- Here, the module only exports the area function
module Geometry.Cube
( area
) where

area :: Float -> Float -> Float -> Float
area x y z = x * y * z

private = 42