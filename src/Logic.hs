-----------------------------------------------------------------------------
--
-- Module      :  Logic
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

module Logic where

import Data.Maybe
import Task
import Etc
import Task
import Interface

addTask::World -> IO World
addTask (World tasks day) = do
    maybeWhenfield <- getName when "When is this task due?"
    maybeRepeatablefield <- getName repeatable "Is this task repeatable?"
    maybeName <- getName name "Name of the task"
    maybeDescription <- getName description "Describe task"

    let maybeWhen = getDateWithValidation (fromJust maybeWhenfield)
    let maybeRepeatable = getRepeValidation (fromJust maybeRepeatablefield)

    if  ( isNothing maybeWhen )
          then do
              putStrLn "Wrong date format! Please use yyyy-mm-dd instead"
              putStrLn ("\n")
          else
              putStr ""

    if isNothing maybeName
            then do putStrLn "maybeName"
            else do putStrLn ""

    if maybeRepeatable == 5
            then do putStrLn "maybeRepeatable"
            else do putStrLn ""

    if isNothing maybeDescription
            then do putStrLn "maybeDescription"
            else do putStrLn ""

    if isNothing maybeWhen ||
       isNothing maybeName ||
       maybeRepeatable == 5 ||
       isNothing maybeDescription
        then do
           putStrLn "Error when adding a task"
           putStrLn ("\n")
           return (World tasks day)
        else do
            let newTaskList = doAddTask   ( getNextTaskId tasks)
                                      (fromJust maybeWhen)
                                       maybeRepeatable
                                      ( fromJust maybeName)
                                      ( fromJust maybeDescription)
                                      False
                                      tasks
                                      
            putStrLn ("\n")
            putStrLn ("Task added successfully!")
            putStrLn ("\n")
            printTask (last newTaskList)  -- nie dziala? :< --------------------
            
            return (World newTaskList day)

doAddTask id when repeatable name description isDone tasks = do
        [(Task id when repeatable name description isDone)] ++ tasks -- [] ??
        
getNextTaskId:: [Task] -> Int
getNextTaskId [] = 1;
getNextTaskId (x:xs) = (max (getTaskId x) (getNextTaskId xs))+1 ;

viewAllTasks::World-> IO World
viewAllTasks (World tasks day) = do
    printTaskList tasks
    return (World tasks day)

viewTasksToDoToday::World-> IO World
viewTasksToDoToday (World tasks day) = do
    printTaskList (filterTasksOnDate tasks day day)
    return (World tasks day)

viewUnfinishedTasks::World-> IO World
viewUnfinishedTasks (World tasks day) = do
    printTaskList (filterFinishedTasks tasks False)
    return (World tasks day)


printTaskList (firstTaskInList:restOfTasks) = do
    printTask firstTaskInList
    printTaskList restOfTasks
    return ()
printTaskList [] = do
    return()

--filterTasksOnDate::[Task]->Day->Day->[Task]
filterTasksOnDate (firstTaskInList:restOfTasks) after before =
        let dayOfTheTask = getTaskWhen firstTaskInList in
                if dayOfTheTask > after ||
                dayOfTheTask < before
                        then filterTasksOnDate restOfTasks after before
                        else firstTaskInList : (filterTasksOnDate restOfTasks after before)
filterTasksOnDate [] _ _ = []

filterFinishedTasks::[Task] -> Bool-> [Task] --not used anywhere so far
filterFinishedTasks (firstTaskInList:restOfTasks) isDone =
    if getTaskIsDone firstTaskInList == isDone
        then firstTaskInList : (filterFinishedTasks restOfTasks isDone)
        else filterFinishedTasks restOfTasks isDone
filterFinishedTasks [] _ = []

removeTask::World-> IO World
removeTask (World tasks day) = do
    putStrLn "Which task do you want to delete?"
    idToDelete <- getInt 0
    --idToDelete  <- getName when "Which task do you want to delete?"  ------- when...
    if (idToDelete == 0)
        then do
            putStrLn "Error: Task id should be a number"
            return (World tasks day)
        else do
            let maybeTask = getTaskById idToDelete tasks --

            if ( isNothing maybeTask )
                then do
                    putStrLn "Error: Task not found"
                    return (World tasks day)
                else do
                    let lessTasks = removeItem (fromJust maybeTask) tasks
                    putStrLn "Task deleted successfully!"
                    return (World lessTasks day)


