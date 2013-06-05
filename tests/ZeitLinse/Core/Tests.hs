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

import ZeitLinse.Core.TimeSpot
import ZeitLinse.Core.WeightedMerging

tests :: Spec
tests = do
  describe "mergeWeighted" $ do
    it "merges scores to their mean" $
      mergeWeighted (constWeights 1 sampleScores) `shouldApproxEq` (Score 0.53)
    it "merges an empty list of scores to nan" $
      mergeWeighted ([] :: [Weighted Score]) `shouldSatisfy` isNaN . fromScore

  where
    sampleScores = map Score [ 0.23, 0.9, 0.42, 0.63, 0, 1 ]
    constWeights c = map (Weighted c)

--
-- Utilities
--
approx :: (Fractional a, Ord a) => a -> a -> Bool
approx a = (<= 1E-15) . abs . (a -)

shouldApproxEq :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldApproxEq actual expected = actual `shouldSatisfy` (approx expected)
