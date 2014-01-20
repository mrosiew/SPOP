-----------------------------------------------------------------------------
--
-- Module      :  Task
-- Copyright   :  Mateusz Rosiewicz i Pawel Sosnowski
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

--Gets current day
getSystemDay::IO Day
getSystemDay = do
        currentTime <- getCurrentTime
        let currentUtcDay = utctDay currentTime in
                let (y,m,d) = toGregorian currentUtcDay in
                        let gregorianDay = fromGregorian y m d in
                                return gregorianDay

--Creates new World that cointains other data       
newWorld::World
newWorld = (World [] (unsafePerformIO getSystemDay))

--Structure containing task list and current day
data World = World {
    tasks::[Task],
    currentDay::Day
    } deriving (Show, Read)

--Structure conteining task-related data
data Task = Task {
    id::Int,
    when::Day,
    repeatable::Int,
    name::String,
    description::String,
    isDone::Bool
} deriving (Show, Read, Eq)

--Gets data form a field
getTaskId(Task id _ _ _ _ _) = id
getTaskWhen(Task _ when _ _ _ _) = when
getTaskRepeatable(Task _ _ repeatable _ _ _) = repeatable
getTaskName(Task _ _ _ name _ _) = name
getTaskDescription( Task _ _ _ _ description _ ) = description
getTaskIsDone(Task _ _ _ _ _ isDone) = isDone
