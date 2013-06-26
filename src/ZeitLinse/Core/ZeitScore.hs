-- ZeitLinse -- time dependent rating of information sources
--
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

-- | Time dependent scores.
module ZeitLinse.Core.ZeitScore
       ( zeitScore
       ) where

import ZeitLinse.Core.Types
import ZeitLinse.Core.WeightedMerging

import Control.Lens.Getter
import Data.Time.Clock

-- | Calculate the score depending on submission time and reference time.
--   Uses simple exponential decay for now, but should be replaced with a
--   smarter algorithm.
zeitScore :: TimedRating -> UTCTime -> Score
zeitScore timedRating referenceTime =
    applyWeight (weightFromTimes (timedRating^.timedRatingTime.fromSubmissionTime)
                                 referenceTime)
                (timedRating^.timedRatingScore)
    where
      weightFromTimes a b = Weight . (exp . negate) . asFloating $ diffUTCTime a b
      asFloating = fromRational . toRational
