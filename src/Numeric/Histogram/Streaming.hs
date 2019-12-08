{-# language RankNTypes  #-}
module Numeric.Histogram.Streaming where

import Control.Monad (void, unless)
import Control.Monad.ST
import Data.Ord (comparing)

-- primitive
import Control.Monad.Primitive (PrimMonad, PrimState)
-- streaming
import qualified Streaming.Prelude as S (Stream, Of, unfoldr)
-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as VMA (sort, sortBy, Comparison)


-- A streaming parallel decision tree algorithm : http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf

-- | A histogram is an array of elements paired with the respective counts
newtype Histo m a = Histo { histo :: VM.MVector m (a, Int) } 

empty :: PrimMonad m => m (Histo (PrimState m) a)
empty = Histo <$> V.thaw V.empty



update x vm = do
  found <- updateIfFound x vm
  unless found $ do
    let l0 = VM.length vm
    vm' <- VM.grow vm 1
    VM.write vm' l0 (x, 1)

updateIfFound :: (PrimMonad m, Eq a) =>
                 a
              -> VM.MVector (PrimState m) (a, Int)
              -> m Bool  -- ^ True if the vector was updated
updateIfFound x vm = do
  go 0
  where
    l = VM.length vm
    go i
      | i < l = do
          (xi, mi) <- VM.read vm i
          if x == xi
            then do
              VM.write vm i (xi, mi + 1)
              pure True
            else go (i + 1)
      | otherwise = pure False


