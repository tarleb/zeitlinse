{-
Copyright (C) 2013–2014 Albert Krewinkel <tarleb@moltkeplatz.de>

This file is part of ZeitLinse.

ZeitLinse is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option)
any later version.

ZeitLinse is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
License for more details.

You should have received a copy of the GNU Affero General Public License
along with ZeitLinse.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Iteresting items we which we want to focus on.
module ZeitLinse.Core
       ( Score(..)
       , fromScore
       , SubmissionTime(..)
       , fromSubmissionTime
       , TimedRating(..)
       , timedRatingScore
       , timedRatingTime
       , TimeSpot(..)
       , timeSpotRating
       , timeSpotFocus
       , Weighted(..)
       , groupTimeSpots
       , mergeWeighted
       , zeitScore
       ) where

import ZeitLinse.Core.Types
import ZeitLinse.Core.WeightedMerging
import ZeitLinse.Core.ZeitScore
