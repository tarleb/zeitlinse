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

{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}

-- | Types for a time-dependend scoring system.
module ZeitLinse.Core.Types where

import Control.Lens
import Data.Function (on)
import Data.Time.Clock
import Data.Time.LocalTime()
import Data.Time.Format

-- The point of time lenses is to rate the importance of an item based on how
-- high other sources rank the item.

-- | Score on the importance of an entry.
newtype Score = Score { _fromScore   :: Double }
              deriving (Eq, Floating, Fractional, Num, Ord, Show)

-- | The time at wich an entry was submitted.
newtype SubmissionTime = SubmissionTime { _fromSubmissionTime :: UTCTime }
                       deriving (Eq, Ord, Read, ParseTime, FormatTime, Show)

-- | A timedScore is a score and the time at which the score was given.
data TimedRating = TimedRating
  { _timedRatingScore      :: Score
  , _timedRatingTime       :: SubmissionTime
  } deriving (Eq, Show)

instance Ord TimedRating where
  -- FIXME: Should include the time of submission
  compare = compare `on` _timedRatingScore

-- | A (possibly time dependent) timedScore of an item is a TimeSpot
data TimeSpot a = TimeSpot
  { _timeSpotRating :: TimedRating
  , _timeSpotFocus  :: a
  } deriving (Eq, Functor, Show)

makeLenses ''Score
makeLenses ''SubmissionTime
makeLenses ''TimedRating
makeLenses ''TimeSpot
