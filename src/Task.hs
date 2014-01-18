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


newWorld = World [] "Poniedzialek"

data World = World {
    tasks::[Task],
    currentDay::String --do implementacji jako dzien
    }

data Task = Task {
    id::Int,
    when::String, --do implementacji jako dzien
    repeatable::Int, --powinien byc jakis smieszny enum
    name::String,
    description::String,
    isDone::Bool
} deriving (Show, Read, Eq)

printTask (Task id when repeatable name description isDone) = do
            putStrLn ("id: " ++ (show id))
            putStrLn ("Due date: " ++ when)
            putStrLn ("Repeatable every " ++ (show id) ++ " day")
            putStrLn ("Name " ++ name)
            putStrLn ("Description: " ++ description)
            putStrLn ("It's done? " ++ (show isDone) )

getTaskId(Task id _ _ _ _ _) = id
getTaskWhen(Task _ when _ _ _ _) = when
getTaskRepeatable(Task _ _ repeatable _ _ _) = repeatable
getTaskName(Task _ _ _ name _ _) = name
getTaskDescription( Task _ _ _ _ description _ ) = description
getTaskIsDone(Task _ _ _ _ _ isDone) = isDone
