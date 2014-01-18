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

    if  ( isNothing maybeRepeatable )
          then do
              showError "jakis string 6"
          else
              putStr ""


    if isNothing maybeWhen ||
       isNothing maybeRepeatable ||
       isNothing maybeName ||
       isNothing maybeDescription
        then do
           putStrLn "jakis string 7"
           return (World tasks day)
        else do
            let newTask = doAddTask   ( getNextTaskId tasks)
                                      ( fromJust maybeWhen)
                                      ( fromJust maybeRepeatable)
                                      ( fromJust maybeName)
                                      ( fromJust maybeDescription)
                                      False
                                      tasks

            putStrLn "jakis string 8" -- sukces!!
            return (World newTask day)


--doAddPerson  id firstName lastName companyName phone email birthDay persons = do
--        [(Person id firstName lastName companyName phone email birthDay [])] ++ persons

doAddTask id when repeatable name description isDone tasks = do
        [(Task id when repeatable name description isDone)] ++ tasks -- [] ??
        
getNextTaskId:: [Task] -> Int
getNextTaskId [] = 1;
getNextTaskId (x:xs) = (max (getTaskId x) (getNextTaskId xs))+1 ;

--newTask = tasks ++ newTask
