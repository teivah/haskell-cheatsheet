module IO
  ( 
  ) where

import Control.Exception (bracket)
import Control.Monad (forM, forever, when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.IO

{---------------}
{----- 101 -----}
{---------------}
-- An I/O action is something that, when performed, will carry out an action
-- with a side effect (e.g., socket read) and will also present some result
main = do
  putStrLn "Hello"
  -- getLine (`getLine :: IO String`) is an I/O action that yields a String
  -- <- maps an IO f into a f (it "opens" the IO box), here name is a String
  -- Main difference with let that binds the result directly to a name
  -- Note: getLine is impure because its result value is not guaranteed to be
  -- the same when performed twice
  name <- getLine
  putStrLn ("Hey " ++ name)
  -- Returns yields an IO f, here an IO String
  -- Note: it doesn't cause the function to stop
  return "foo"
  -- The function continues, IO "foo" is simply not bound to a name
  -- a is an IO String
  a <- return "bar"
  return "end"

{----------------}
{----- When -----}
{----------------}
-- when takes a Bool and an I/O action:
-- * If true, it returns the same I/O
-- * If false, it returns ()
withWhen = do
  input <- getLine
  when (input == "foo") $ do
    putStrLn input

-- The same without a when
withoutWhen = do
  input <- getLine
  if (input == "foo")
    then putStrLn input
    else return ()

{--------------------}
{----- Sequence -----}
{--------------------}
-- sequence takes a list of I/O actions and returns an I/O action that will
-- perform those actions one after the other
withSequence = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- The same without a sequence
withoutSequence = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a, b, c]

{-----------------------}
{----- MapM / ForM -----}
{-----------------------}
-- mapM takes a function and a list and maps the function over the list
withMapM = do
  mapM print [1, 2, 3]

-- forM takes a list first and then a function
withForM = do
  forM [1, 2, 3] print

{-------------------}
{----- Forever -----}
{-------------------}
-- forever takes an I/O action and returns an I/O action that just repeats the
-- I/O action forever
withForever =
  forever $ do
    l <- getLine
    putStrLn l

{----------------}
{----- File -----}
{----------------}
-- Open a file
openFile = do
  withFile
    "foo.txt"
    ReadMode
    (\handle -> do
      -- hGetContents works like getContents but for a specific file
      -- Note: functions like getLine, putStr, putStrLn and so on have their h
      -- counterparts
       contents <- hGetContents handle
       putStr contents)

-- Bracket
-- When we acquire some resource and we want to make sure the resource gets
-- released (e.g., file handle is closed), we can use the bracket function
-- Signature:
-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- Note: bracketOnError performs a cleanup only if an exception has been raised
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (System.IO.openFile name mode) (\handle -> hClose handle) (\handle -> f handle)

-- Lazy I/O example
-- Nothing is eaten until it absolutely must be
-- This gives us this streaming example
streamingEx = do
  contents <- readFile "foo.txt"
  writeFile "output.txt" (map toUpper contents)

{-----------------------}
{----- CLI example -----}
{-----------------------}
dispatch :: String -> [String] -> IO ()
dispatch "foo" = foo
dispatch "bar" = bar

foo :: [String] -> IO ()
foo _ = return ()

bar :: [String] -> IO ()
bar _ = return ()

main' = do
  (command:argList) <- getArgs
  dispatch command argList

{-----------------------}
{----- Bytestrings -----}
{-----------------------}
-- A list is lazy by nature, the next element will be delivered once it has to
-- be
-- When reading big files, we can use bytestrings: acts like lists but each
-- element is one byte in size
-- Comes in two flavors:
-- * Strict: Data.ByteString, evaluate all at once
-- * Lazy: Data.ByteString.Lazy, evaluation is done by chunks of 64KB
--
-- Use case: when a program reads a lot of data into strings, using bytestring
-- can give a performance boost
packUnpack = do
  -- Takes a list of bytes and turn it into a bytestring
  let s = B.pack [99, 97, 110] -- "can"
  -- Takes a bytestring and turns it into a list of bytes
  let v = B.unpack s
  -- cons is the bytestring version of :
  B.cons 85 $ B.pack [80, 81, 82, 84]
