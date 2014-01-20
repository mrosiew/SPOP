-----------------------------------------------------------------------------
--
-- Module      :  Task
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Task where

import Data.Time
import Data.Maybe
import Data.Char
import Numeric
import Data.Time.Calendar
import System.IO.Unsafe --35th line

--import Time
--import System.Locale (defaultTimeLocale)

getSystemDay::IO Day
getSystemDay = do
        currentTime <- getCurrentTime
        let currentUtcDay = utctDay currentTime in
                let (y,m,d) = toGregorian currentUtcDay in
                        let gregorianDay = fromGregorian y m d in
                                return gregorianDay
       
newWorld::World
newWorld = (World [] (unsafePerformIO getSystemDay)) --yolo


data World = World {
    tasks::[Task],
    currentDay::Day --do implementacji jako dzien
    } deriving (Show, Read)

data Task = Task {
    id::Int,
    when::Day, --do implementacji jako dzien
    repeatable::Int, --powinien byc jakis smieszny enum
    name::String,
    description::String,
    isDone::Bool
} deriving (Show, Read, Eq)

getTaskId(Task id _ _ _ _ _) = id
getTaskWhen(Task _ when _ _ _ _) = when
getTaskRepeatable(Task _ _ repeatable _ _ _) = repeatable
getTaskName(Task _ _ _ name _ _) = name
getTaskDescription( Task _ _ _ _ description _ ) = description
getTaskIsDone(Task _ _ _ _ _ isDone) = isDone
