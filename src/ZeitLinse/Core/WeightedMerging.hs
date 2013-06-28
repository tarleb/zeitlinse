-- ZeitLinse, time dependent rating of information sources
-- Copyright (C) 2013 Albert Krewinkel <tarleb@moltkeplatz.de>
--
-- This file is part of ZeitLinse.
--
-- ZeitLinse is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- ZeitLinse is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with ZeitLinse.  If not, see <http://www.gnu.org/licenses/>.

{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language ExistentialQuantification #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverlappingInstances #-}
{-# Language TemplateHaskell #-}

module ZeitLinse.Core.WeightedMerging
       (
         Weighted(..)
       , Weight(..)
       , MergeableWeighted(..)
       , groupTimeSpots
       , applyWeight
       ) where

import ZeitLinse.Core.Types

import Prelude hiding (minimum, foldr)
import Control.Lens
import Data.Foldable
import Data.Function (on)
import Data.Traversable
import qualified Data.Map as M

-- | Mergeable formalizes our need to combine multiple entities into one.
class MergeableWeighted a where
  mergeWeighted  :: Traversable f => f (Weighted a) -> a

-- | A weight can be any real number, but will usually be in the interval [-1,1]
newtype Weight = Weight { fromWeight :: Double }
               deriving (Eq, Num, Fractional, Ord, Show)

-- | Importance is relative and can be weighted.
data Weighted a =
  Weighted
  { _weight             :: Weight
  , _unweightedItem     :: a
  } deriving (Eq, Ord, Functor, Show, Foldable, Traversable)

makeLenses ''Weighted

-- Treat weighted items like scaled vectors
applyWeight' :: Fractional a => Weighted a -> a
applyWeight' (Weighted w a) = applyWeight w a

applyWeight :: Fractional a => Weight -> a -> a
applyWeight w a = (realToFrac . fromWeight $ w) * a

--
-- Various Mergeable instances
--

-- | Helper for recursive merging of data structures.
submergeWeighted :: forall a b t.
                    (MergeableWeighted a, MergeableWeighted b, Traversable t) =>
                    (a -> b) -> t (Weighted a) -> b
submergeWeighted fn = mergeWeighted . fmap (fmap fn)

instance MergeableWeighted Score where
  mergeWeighted = mean . fmap applyWeight'

instance MergeableWeighted SubmissionTime where
  mergeWeighted = minimum . fmap _unweightedItem

instance MergeableWeighted TimedRating where
  -- we merge time and scores independently for now.
  mergeWeighted rs = TimedRating (mergeScores rs) (mergeTime rs)
    where
      mergeScores = submergeWeighted _timedRatingScore
      mergeTime   = submergeWeighted _timedRatingTime

instance MergeableWeighted (TimeSpot a) where
  -- just take the item with the highest weight and merge the scores into a
  -- new score.
  mergeWeighted ts = TimeSpot mergedRating bestFocalItem
    where
      bestFocalItem = submergeWeighted _timeSpotFocus  ts
      mergedRating  = submergeWeighted _timeSpotRating ts

instance MergeableWeighted b where
  mergeWeighted = _unweightedItem . maximumBy (compare `on` _weight)


-- Helpers and Utillities
--

mean :: (Foldable f, Fractional a) => f a -> a
mean = uncurry (/) . foldr go (0,0)
  where go x (s,n) = (s+x, n+1)


-- | Group TimeSpots with similar focal items together.
groupTimeSpots :: (Foldable f, Ord a) =>
                  f (Weighted (TimeSpot a)) -> [[Weighted (TimeSpot a)]]
groupTimeSpots = M.elems . foldr step M.empty
  where step a b = M.insertWith (++) (key a) [a] b
        key = _timeSpotFocus . _unweightedItem
