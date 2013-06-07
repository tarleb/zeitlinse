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

{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}
module ZeitLinse.Core.Types
       ( Score(..)
       , TimedScore(..)
       , TimeSpot(..)
       ) where

import Data.Time.Clock (UTCTime(..))

-- The point of time lenses is to rate the importance of an item based on how
-- high other sources rank the item.

-- | Score on the importance of an entry.
newtype Score = Score { fromScore   :: Double }
              deriving (Eq, Floating, Fractional, Num, Ord, Show)

-- | A timedScore is a score and the time at which the score was given.
data TimedScore = TimedScore
  { _score      :: Score
  , _time       :: UTCTime
  } deriving (Eq, Show)

instance Ord TimedScore where
  -- FIXME: Should include the time of submission
  a `compare` b = (_score a) `compare` (_score b)

-- | A (possibly time dependent) timedScore of an item is a TimeSpot
data TimeSpot a = TimeSpot
  { _timedScore :: TimedScore
  , _focalItem  :: a
  } deriving (Eq, Functor, Show)
