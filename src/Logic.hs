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

addTask::World -> IO World
addTask (World tasks day) = do
    maybeWhenfield <- getName when "jakis string"
    maybeRepeatablefield <- getName repeatable "jakis string 2"
    maybeName <- getName name "jakis string 3"
    maybeDescription <- getName description "jakis string 4"

    let maybeWhen = getDateWithValidation (fromJust maybeWhenfield)
    let maybeRepeatable = getRepeValidation (fromJust maybeRepeatablefield)

    if  ( isNothing maybeWhen )
          then do
              showError "jakis string 5"
          else
              putStr ""

 --   if  ( isNothing maybeRepeatable )
  --        then do
 --             showError "jakis string 6"
 --         else
 --             putStr ""


    if isNothing maybeWhen ||
       isNothing maybeName ||
       isNothing maybeDescription
        then do
           putStrLn "Puste cus"
           return (World tasks day)
        else do
            let newTaskList = doAddTask   ( getNextTaskId tasks)
                                      ( show (fromJust maybeWhen))
                                       maybeRepeatable
                                      ( fromJust maybeName)
                                      ( fromJust maybeDescription)
                                      False
                                      tasks

            putStrLn "Udalo sie dodac" -- sukces!!
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

printTaskList (firstTaskInList:restOfTasks) = do
    printTask firstTaskInList
    printTaskList restOfTasks
    return ()
printTaskList [] = do
    return()

