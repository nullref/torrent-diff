module ConcatMapM (concatMapM) where

import Control.Monad
  ( liftM
  )

-- | As mapM, with the resulting lists concatenated.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `liftM` mapM f xs
