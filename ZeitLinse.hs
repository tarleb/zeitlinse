-- ZeitLinse, time dependent rating of information sources
-- Copyright (C) 2013 Albert Krewinkel <tarleb@moltkeplatz.de>

-- This file is part of ZeitLinse.

-- ZeitLinse is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.

-- ZeitLinse is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with ZeitLinse.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE
    DeriveFunctor,
    ExistentialQuantification,
    FlexibleInstances,
    GeneralizedNewtypeDeriving,
    OverlappingInstances,
    StandaloneDeriving #-}
module ZeitLinse where

import Prelude hiding (minimum, foldr)
import Control.Arrow ((***), (&&&))
import Data.Foldable
import Data.Traversable

import qualified Data.Map as M

-- The point of time lenses is to rate the importance of an item based on how
-- high other sources rank the item.

-- | Score on the importance of an entry.
newtype Score = Score { fromScore :: Double }
                deriving (Eq, Floating, Fractional, Num, Ord, Show)

-- | The time at which an entry was submitted
newtype SubmissionTime = SubmissionTime { fromSubmissionTime :: Integer}
                         deriving (Eq, Ord, Show)

-- | A timedScore is a score and the time at which the score was given.
data TimedScore = TimedScore
              { _score :: Score
              , _time :: SubmissionTime
              }
              deriving (Eq, Show)

instance Ord TimedScore where
  -- FIXME: Should include the time of submission
  a `compare` b = (_score a) `compare` (_score b)
  
-- | A (possibly time dependent) timedScore of an item is a TimeSpot
data TimeSpot a = TimeSpot { _timedScore :: TimedScore
                           , _focalItem :: a
                           }
                deriving (Eq, Show)
deriving instance Functor TimeSpot

-- | Mergeable formalizes our need to combine multiple entities into one.
--
class MergeableWeighted a where
  mergeWeighted :: Traversable f => f (Weighted a) -> a


-- | Importance is relative and can be weighted.
data Weighted a = Weighted { _weight :: Weight
                           , _unweightedItem :: a
                           } deriving (Eq, Ord, Show)

newtype Weight = Weight { fromWeight :: Double }
               deriving (Eq, Num, Fractional, Ord, Show)

deriving instance Functor Weighted

-- Treat weighted items like scaled vectors
applyWeight :: Fractional a => Weighted a -> a
applyWeight (Weighted w a) = applyWeight' w a

applyWeight' w a = (realToFrac . fromWeight $ w) * a

--
-- Various Mergeable instances
--
submergeWeighted :: forall a b t.
                    (MergeableWeighted a, MergeableWeighted b, Traversable t) =>
                    (a -> b) -> t (Weighted a) -> b
submergeWeighted fn = mergeWeighted . fmap (fmap fn)

instance MergeableWeighted Score where
  mergeWeighted = mean . fmap applyWeight

instance MergeableWeighted SubmissionTime where
  mergeWeighted = minimum . fmap _unweightedItem

instance MergeableWeighted TimedScore where
  -- we merge time and scores independently for now.
  mergeWeighted rs = TimedScore (mergeScores rs) (mergeTime rs)
    where
      mergeScores = submergeWeighted _score
      mergeTime   = submergeWeighted _time

instance MergeableWeighted (TimeSpot a) where
  -- just take the item with the highest weight and merge the scores into a
  -- new score.
  mergeWeighted ts = TimeSpot mergedScore bestFocalItem
    where
      bestFocalItem = submergeWeighted _focalItem  ts
      mergedScore   = submergeWeighted _timedScore ts

instance MergeableWeighted b where
  mergeWeighted = _unweightedItem . maximumBy compareWeight
    where compareWeight = curry $ uncurry compare . (_weight *** _weight)


-- Helpers and Utillities
--

mean :: (Foldable f, Fractional a) => f a -> a
mean = uncurry (/) . foldr go (0,0)
  where go x (s,n) = (s+x, n+1)

--
-- Entries
--
type Title = String
newtype Resource = Resource String
                 deriving (Eq, Ord, Show)

data Article = Article Title Resource
             deriving (Eq, Ord, Show)

-- -- ---------------------------------------------------------------------------

sampleArticles = [ Article "Article 1" (Resource "justSomeResource")
                 , Article "Article 2" (Resource "Hello, World!")
                 , Article "Nice Things" (Resource "reddit.com")
                 , Article "This is new" (Resource "NY times")
                 ]

sampleScores = map Score [ 0.23 , 0.9 , 0.42 , 0.63 , 0 , 1]
sampleSubmissionTimes = map SubmissionTime [ 5 , 46 , 71 , 36]
sampleTimedScores = zipWith TimedScore sampleScores sampleSubmissionTimes
sampleTimeSpots = zipWith (TimeSpot) sampleTimedScores sampleArticles

main = do
  print . mergeWeighted $
    zipWith Weighted
            [0.7, 0.68, 0.98, 0.2, 0.13, 0.65]
            sampleTimeSpots
                         
