module Others
  ( 
  ) where

import Debug.Trace

-- Logging
tracingEx = trace (show (foo)) $ foo
  where
    foo = 42
