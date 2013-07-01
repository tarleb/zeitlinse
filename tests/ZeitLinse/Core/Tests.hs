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

module ZeitLinse.Core.Tests
       ( tests
       ) where

import Test.Hspec
import Test.QuickCheck

import ZeitLinse.Core

import Control.Applicative ((<$>), (<*>))
import Control.Lens.Getter
import Control.Lens.Setter
import Data.Foldable (toList)
import Data.Time.Clock
import Data.Time.Calendar

-- FIXME: These tests are horrible
tests :: Spec
tests = do
  describe "mergeWeighted" $ do
    it "merges scores to their mean" $
       let sampleScores   = map Score [ 0.23, 0.9, 0.42, 0.63, 0, 1 ]
           constWeights c = map (Weighted c)
       in mergeWeighted (constWeights 1 sampleScores) `shouldApproxEq` (Score 0.53)

    it "merges an empty list of scores to nan" $
       let emptyList = [] :: [Weighted Score]
       in mergeWeighted emptyList `shouldSatisfy` isNaN . (^.fromScore)

  describe "group TimeSpots" $ do
    it "groups TimeSpots with the same focus items into a single group" $
       let eqlTimeSpots = take 3 . repeat $ (Weighted 0.5 sampleTimeSpot)
       in groupTimeSpots eqlTimeSpots `shouldSatisfy` ((== 1) . length . toList)

    it "groups TimeSpots with different focus items into different groups" $
      groupTimeSpots [ Weighted 0.8 sampleTimeSpot
                     , Weighted 0.7 sampleTimeSpot
                     , Weighted 0.5 (set timeSpotFocus "Moin" sampleTimeSpot) ]
      `shouldSatisfy` ((== 2) . length . toList)

  describe "zeitScore" $ do
    it "gives decreasing scores over time" $
      quickCheck (prop_zeitScore_decreases_with_time sampleTimedRating)

    it "gives the original score at the time of submission" $
       zeitScore sampleTimedRating sampleTime `shouldApproxEq`
                     (sampleTimedRating^.timedRatingScore)

  where
    sampleTime        = (read "2013-08-03 12:00:00 UTC") :: UTCTime
    sampleTimedRating = TimedRating 0.5 . SubmissionTime $ sampleTime
    sampleTimeSpot    = TimeSpot sampleTimedRating "Hello"

--
-- Utilities
--
approx :: (Fractional a, Ord a) => a -> a -> Bool
approx a = (<= 1E-15) . abs . (a -)

shouldApproxEq :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldApproxEq actual expected = actual `shouldSatisfy` (approx expected)

--
-- QuickCheck
--

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary NominalDiffTime where
    arbitrary = (fromRational . toRational) <$> (arbitrary :: Gen Double)

prop_zeitScore_decreases_with_time :: TimedRating -> NominalDiffTime -> Property
prop_zeitScore_decreases_with_time timeScore refTime =
    refTime >= 0 ==>
         zeitScore timeScore afterSubmissionUTCTime <= (timeScore^.timedRatingScore)
    where submissionUTCTime = timeScore^.timedRatingTime.fromSubmissionTime
          afterSubmissionUTCTime = refTime `addUTCTime` submissionUTCTime
