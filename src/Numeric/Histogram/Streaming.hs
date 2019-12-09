{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
module Numeric.Histogram.Streaming where

import Control.Monad (void, unless, foldM)
import Control.Monad.ST
-- import Data.Foldable (for_)
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
newtype Histo m a = Histo {
  histo :: VM.MVector m (a, Int)
  } 

-- empty :: PrimMonad m => m (Histo (PrimState m) a)
-- empty = Histo <$> V.thaw V.empty 







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


updateIfNotFound normf x vm = do
  let l0 = VM.length vm
  vm' <- VM.grow vm 1
  VM.write vm' l0 (x, 1) -- append new element 
  VMA.sortBy (comparing (normf . fst)) vm' -- sort by magnitude

-- overwrites the length (l + 1) temporary state of the histogram
--
-- UNSAFE
overwrite fdist fsum fsm vm = do
  iforM_ insf vm
  pure $ VGM.init vm
  where
    ((x1, x2), icut) = mutIndex fdist vm
    x' = newBin fsum fsm x1 x2
    insf i x
      | i < icut = VGM.write vm i x
      | i == icut = VGM.write vm i x'
      | otherwise = do
          xsucc <- VGM.read vm (i + 1)
          VGM.write vm i xsucc


iforM_ :: (Monad m, VG.Vector v t) => (Int -> t -> m ()) -> v t -> m ()
iforM_ f = VG.ifoldM'_ insf () where
  insf _ i x = f i x 

mutIndex :: (Ord a, VG.Vector v Int, VG.Vector v x
            , VG.Vector v (x, x), VG.Vector v ((x, x), Int)) =>
            (x -> x -> a)
         -> v x
         -> ((x, x), Int) -- ^ ((x_i, x_i+1), i)
mutIndex fdist v = iMinimumBy (uncurry fdist) vd
  where
    vd = VG.zip (VG.init v) (VG.tail v)

newBin :: (Fractional t1, Integral b) =>
          (t2 -> t2 -> a) -- ^ vector sum
       -> (t1 -> a -> t2) -- ^ scalar-vector multiplication
       -> (a, b)
       -> (a, b)
       -> (t2, b)
newBin (^+^) (.*) (q1, k1) (q2, k2) = (q', k')
  where
    k' = k1 + k2
    q' = recip (fromIntegral k') .* ((fromIntegral k1 .* q1) ^+^ (fromIntegral k2 .* q2))

iMinimumBy :: (Ord a, VG.Vector v b1, VG.Vector v b2, VG.Vector v (b1, b2),
               Num b2, Enum b2) =>
              (b1 -> a) -> v b1 -> (b1, b2)
iMinimumBy fnorm v = VG.minimumBy (comparing (fnorm . fst)) $ VG.zip v (VG.fromList [0 .. ])






