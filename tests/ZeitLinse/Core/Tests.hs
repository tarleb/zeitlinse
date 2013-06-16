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
import Data.Foldable (toList)
import Data.Time.Clock
import Data.Time.Calendar

-- FIXME: These tests are horrible
tests :: Spec
tests = do
  describe "mergeWeighted" $ do
    it "merges scores to their mean" $
      mergeWeighted (constWeights 1 sampleScores) `shouldApproxEq` (Score 0.53)

    it "merges an empty list of scores to nan" $
      mergeWeighted ([] :: [Weighted Score]) `shouldSatisfy` isNaN . fromScore

  describe "group TimeSpots" $ do
    it "groups TimeSpots with the same focus items into a single group" $
      groupTimeSpots equalTimeSpots `shouldSatisfy` ((== 1) . length . toList)

    it "groups TimeSpots with different focus items into different groups" $
      groupTimeSpots [ Weighted 0.8 sampleTimeSpot
                     , Weighted 0.7 sampleTimeSpot
                     , Weighted 0.5 sampleTimeSpot { _focalItem = "Moin" } ]
      `shouldSatisfy` ((== 2) . length . toList)
  describe "zeitScore" $ do
    it "gives decreasing scores over time" $
      quickCheck (prop_zeitScore_decreases_with_time sampleTimedScore)


  where
    sampleTimedScore = TimedScore 0.5 . SubmissionTime . read $
                       "2013-08-03 12:00:00 UTC"
    sampleTimeSpot = TimeSpot sampleTimedScore "Hello"
    equalTimeSpots = take 3 . repeat $ (Weighted 0.5 sampleTimeSpot)
    sampleScores = map Score [ 0.23, 0.9, 0.42, 0.63, 0, 1 ]
    constWeights c = map (Weighted c)

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

prop_zeitScore_decreases_with_time :: TimedScore -> NominalDiffTime -> Property
prop_zeitScore_decreases_with_time timedScore time =
    time >= 0 ==>
         zeitScore timedScore afterSubmissionUTCTime <= (_score timedScore)
    where submissionUTCTime = fromSubmissionTime . _time $ timedScore
          afterSubmissionUTCTime = time `addUTCTime` submissionUTCTime
