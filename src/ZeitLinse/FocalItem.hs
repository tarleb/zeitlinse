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

-- | Types for iteresting items we want to focus on.
module ZeitLinse.FocalItem
       ( FocalItem(..)
       , Resource(..)
       , Title
       , Url
       ) where

--
-- Entries
--
type Title = String

type Url = String

data Resource = RSSFeed Url
              | Journal String
              | WebSite Url
              deriving (Eq, Ord, Show)

data FocalItem = Article Title Resource
               | Task Title (Maybe Resource)
               deriving (Eq, Ord, Show)

{-
sampleArticles = [ Article "F1rst P0st" (RSSFeed "https://blog.example.com/")
                 , Article "Foobar" (Journal "Journal of Stupid Examples")
                 , Article "Where did my time go?" (WebSite "http://reddit.com")
                 , Task    "Carpe diem" Nothing
                 ]
-}
