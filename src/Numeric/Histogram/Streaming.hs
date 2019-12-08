{-# language RankNTypes  #-}
module Numeric.Histogram.Streaming where

import Control.Monad.ST
import Data.Ord (comparing)

-- primitive
import Control.Monad.Primitive (PrimMonad, PrimState)
-- streaming
import qualified Streaming.Prelude as S (Stream, Of, unfoldr)
-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


-- A streaming parallel decision tree algorithm : http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf

data Histo m a = Histo {
  histoBufferMain :: VM.MVector m a, 
  histoBufferTemp :: VM.MVector m a
                     } 

new :: PrimMonad m => Int -> m (Histo (PrimState m) a)
new n = Histo <$> VM.new n <*> VM.new (n + 1)

