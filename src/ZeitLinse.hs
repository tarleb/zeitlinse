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

module ZeitLinse where

import ZeitLinse.Core.Types
import ZeitLinse.Core.WeightedMerging

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
                         
