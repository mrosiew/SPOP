-----------------------------------------------------------------------------
--
-- Module      :  Logic
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

module Logic where

import Data.List
import Data.Maybe
import Task
import Etc
import Task
import Interface
import Data.Time.Calendar

--Gets data needed to create a new task
addTask::World -> IO World
addTask (World tasks day) = do
    maybeWhenfield <- getName when "When is this task due? (yyyy-mm-dd)"
    --maybeHourfield <- getName hour "placeholder"
    maybeRepeatablefield <- getName repeatable "Is this task repeatable? ('no, 'daily, 'weekly' or 'monthly', 'yearly')"
    maybeName <- getName name "Name of the task"
    maybeDescription <- getName description "Describe task"

    let maybeWhen = getDateWithValidation (fromJust maybeWhenfield)
    let maybeRepeatable = getRepeValidation (fromJust maybeRepeatablefield)
    --let maybeHour = words (fromJust maybeHourfield)
    --let newHour = parseHour maybeHour 2
    
    if  ( isNothing maybeWhen )
          then do
              putStrLn "Wrong date format! Please use yyyy-mm-dd instead"
              putStrLn ("\n")
          else
              putStr ""

    if isNothing maybeName
            then do putStrLn "Wrong Name value"
            else do putStrLn ""

    if maybeRepeatable == 5
            then do putStrLn "Wrong Repeatable value"
            else do putStrLn ""

    if isNothing maybeDescription
            then do putStrLn "Wrong Description value"
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

--Adds new task to tasks list
doAddTask::Int->Day->Int->String->String->Bool->[Task]->[Task]
doAddTask id when repeatable name description isDone tasks = do
        [(Task id when repeatable name description isDone)] ++ tasks -- [] ??

--Gets first free id        
getNextTaskId:: [Task] -> Int
getNextTaskId [] = 1;
getNextTaskId (x:xs) = (max (getTaskId x) (getNextTaskId xs))+1 ;

--Views all tasks from tasks list
viewAllTasks::World-> IO World
viewAllTasks (World tasks day) = do
    printTaskList tasks
    return (World tasks day)

--Views task with today as it's due date
viewUpdatedTasksToDoToday::World->IO World
viewUpdatedTasksToDoToday world = viewTasksToDoToday (updateEverything world)

--Helper fucntion used to display tasks
viewTasksToDoToday::World-> IO World
viewTasksToDoToday (World tasks day) = do
    let taskList = filterTasksOnDate tasks day day
    if taskList == [] then do putStrLn ("\n")
                              putStrLn "No tasks to display"
                              putStrLn ("\n")
                              return (World tasks day)
                      else do printTaskList taskList
                              return (World tasks day)

--Views tasks with specific due date
viewUpdatedTasksToDoOnX::World->IO World
viewUpdatedTasksToDoOnX world = viewTasksToDoOnX (updateEverything world)

--Helper fucntion used to display tasks
viewTasksToDoOnX::World-> IO World
viewTasksToDoOnX (World tasks day) = do
    maybeDayfield <- getName when "What date do you want to check? (yyyy-mm-dd)"
    
    let maybeDay = getDateWithValidation (fromJust maybeDayfield)

    if  ( isNothing maybeDay )
          then do
              putStrLn "Wrong date format! Please use yyyy-mm-dd instead"
              putStrLn ("\n")
              return (World tasks day)
          else do
              let taskList = filterTasksOnDate tasks (fromJust maybeDay) (fromJust maybeDay)
              if taskList == [] then do putStrLn ("\n")
                                        putStrLn "No tasks to display"
                                        putStrLn ("\n")
                                        return (World tasks day)
                                else do printTaskList taskList
                                        return (World tasks day)

--Views Unfinished tasks
viewUnfinishedTasks::World-> IO World
viewUnfinishedTasks (World tasks day) = do
    printTaskList (filterFinishedTasks tasks False)
    return (World tasks day)

--Prints tasks
printTaskList (firstTaskInList:restOfTasks) = do
    printTask firstTaskInList
    printTaskList restOfTasks
    return 1
printTaskList [] = do
    return 0

--Returns a list of task with specific due date
filterTasksOnDate (firstTaskInList:restOfTasks) after before =
        let dayOfTheTask = getTaskWhen firstTaskInList in
                if dayOfTheTask > after ||
                dayOfTheTask < before
                        then filterTasksOnDate restOfTasks after before
                        else firstTaskInList : (filterTasksOnDate restOfTasks after before)
filterTasksOnDate [] _ _ = []

--Returns a list of finished tasks
filterFinishedTasks::[Task] -> Bool-> [Task]
filterFinishedTasks (firstTaskInList:restOfTasks) isDone =
    if getTaskIsDone firstTaskInList == isDone
        then firstTaskInList : (filterFinishedTasks restOfTasks isDone)
        else filterFinishedTasks restOfTasks isDone
filterFinishedTasks [] _ = []

--Function used to remove a task from tasks list
removeTask::World-> IO World
removeTask (World tasks day) = do
    putStrLn "Which task do you want to delete?"
    idToDelete <- getInt 0
    if (idToDelete == 0)
        then do
            putStrLn "Error: Task id should be a number"
            return (World tasks day)
        else do
            let maybeTask = getTaskById idToDelete tasks
            if ( isNothing maybeTask )
                then do
                    putStrLn "Error: Task not found"
                    return (World tasks day)
                else do
                    let lessTasks = removeItem (fromJust maybeTask) tasks
                    putStrLn "Task deleted successfully!"
                    putStrLn ("\n")
                    return (World lessTasks day)

--Function used to mark tasks as done
markAsDone::World-> IO World
markAsDone (World tasks day) = do
    putStrLn "Which task do you want to mark as done?"
    idToDone <- getInt 0
    if (idToDone == 0)
        then do
            putStrLn "Error: Task id should be a number"
            return (World tasks day)
        else do
            let maybeTask = getTaskById idToDone tasks
            if ( isNothing maybeTask )
                then do
                    putStrLn "Error: Task not found"
                    return (World tasks day)
                else do
                    let task = (fromJust maybeTask)
                    let index = fromJust (elemIndex task tasks)
                    let doneTask =  (Task (getTaskId task)
                                    (getTaskWhen task)
                                    (getTaskRepeatable task)
                                    (getTaskName task)
                                    (getTaskDescription task)
                                    True)
                    let changedTasks = replaceTask index doneTask tasks
                    putStrLn "Task status changed successfully!"
                    putStrLn ("\n")
                    return (World changedTasks day)

--Function used to change current date
changeDate (World tasks day) = do
    maybeDayfield <- getName when "What date do you want to set? (yyyy-mm-dd)"

    let maybeDay = getDateWithValidation (fromJust maybeDayfield)
    
    if  ( isNothing maybeDay )
          then do
              putStrLn "Wrong date format! Please use yyyy-mm-dd instead"
              putStrLn ("\n")
              return (World tasks day)
          else do
              putStrLn "Current day changed successfully!"
              putStrLn ("\n")
              return (World tasks (fromJust maybeDay))

--Frunction returns current date
todaysDate (World tasks day) = day

--Function used to change task data
modifyTask::World->IO World
modifyTask (World tasks day) = do
    putStrLn "Which task do you want to modify?"
    idMod <- getInt 0
    if (idMod == 0)
        then do
            putStrLn "Error: Task id should be a number"
            return (World tasks day)
        else do
            let maybeTask = getTaskById idMod tasks
            if ( isNothing maybeTask )
                then do
                    putStrLn "Error: Task not found"
                    return (World tasks day)
                else do
                    let task = fromJust maybeTask
                    let index = fromJust (elemIndex task tasks)
                    let newTaskList = modifyTaskMenu task tasks
                    putStrLn "Task status changed successfully!"
                    putStrLn ("\n")
                    newTaskList >>= (\ x -> liftWorld (return (snd x)) (return day))

--liftWorld :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--liftWorld::IO m => IO [Task] -> IO Day -> IO World
--helper fucntion creating new world
liftWorld  changedTasks day = do
        x <- changedTasks
        y <- day
        return (World x y)

--modifyTaskMenu::Task->[Task]->IO [Task]
--Defines modify task menu
modifyTaskMenu task taskList = do
        pickedTask <- menu [("Modify task's due date", modDate),
                            ("Modify how often the task will be repeated", modRepe), --modRepe),
                            ("Modify task's name",modName), --modName),
                            ("Modify task's description", modDesc), --modDesc),
                            ("Go back to menu",emp)]
        kek <- pickedTask (task, taskList) --
        return kek

--Filler function
emp (task, taskList) = do return (task, taskList)

--modDate::Task->[Task]->IO [Task]
--Modifies task's due date
modDate ((Task id when repeatable name description isDone), tasks) = do
    maybeWhenfield <- getName when "When is this task due? (yyyy-mm-dd)"
    let maybeWhen = getDateWithValidation (fromJust maybeWhenfield)
    let oldTask = Task id when repeatable name description isDone
    if isNothing maybeWhen 
          then do
                putStrLn "Wrong date format! Please use yyyy-mm-dd instead"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id (fromJust maybeWhen) repeatable name description isDone) in
                                return (oldTask, (replaceTask index newTask tasks))
--Modifies task's repeatability
modRepe ((Task id when repeatable name description isDone), tasks) = do
    maybeRepeatablefield <- getName repeatable "Is this task repeatable? ('no, 'daily, 'weekly', 'monthly' or 'yearly')"
    let maybeRepeatable = getRepeValidation (fromJust maybeRepeatablefield)
    let oldTask = Task id when repeatable name description isDone
    if  ( maybeRepeatable == 5 )
          then do
                putStrLn "Wrong repetition value!"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id when maybeRepeatable name description isDone) in
                                return (oldTask, (replaceTask index newTask tasks))

--Modifies task's name
modName ((Task id when repeatable name description isDone), tasks) = do
    maybeName <- getName name "Name of the task"
    let oldTask = Task id when repeatable name description isDone
    if  ( isNothing maybeName )
          then do
                putStrLn "No name value!"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id when repeatable (fromJust maybeName) description isDone) in
                                return (oldTask, (replaceTask index newTask tasks))
--Modifies task's description
modDesc ((Task id when repeatable name description isDone), tasks) = do
    maybeDescription <- getName description "Describe task"
    let oldTask = Task id when repeatable name description isDone
    if  ( isNothing maybeDescription )
          then do
                putStrLn "No description value!"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id when repeatable name (fromJust maybeDescription) isDone) in
                                return (oldTask, (replaceTask index newTask tasks))

--Function used for making copies of repeatable task (permanent)
updateEverythingMenu (World tasks day) =
        return (World (doUpdateEverything(World tasks day) (getNextTaskId tasks)) day)

--Helper function used for making copies of repeatable task
updateEverything (World tasks day) = World (doUpdateEverything(World tasks day) (getNextTaskId tasks)) day

--Function used for making copies of repeatable task 
doUpdateEverything (World [] day) _ = []
doUpdateEverything (World (taskHead:restOfList) day) givenId =
        let updatedList = (updateRepeatable taskHead day givenId) in
        updatedList ++ (doUpdateEverything (World restOfList day) (getNextTaskId updatedList))


--Changes task's repeatability to 'no'
updateRepeatable (Task id when repeatable name description isDone) day maxId =
        let oldTask = Task id when repeatable name description isDone in
        if repeatable == 0 || when > day
                then [oldTask]
                else
                        let oldUpdatedTask = (Task id when 0 name description isDone) in
                        let newTaskPattern = (Task maxId when repeatable name description False) in
                            oldUpdatedTask:(addRepeatables newTaskPattern day)

--Adds repeatable task copies
addRepeatables (Task givenId when repeatable name description isDone) day  =
        let newDay = getNewDay repeatable when in
        if (newDay <= day) then
                let furtherList = (addRepeatables (Task (givenId+1) newDay repeatable name description False) day) in
                let newTask = (Task givenId newDay 0 name description False) in
                        newTask : furtherList
            else
                let newTask = (Task givenId newDay repeatable name description False) in
                        [newTask]
                        
--Helper function getting new days for repeatable tasks
getNewDay repeatable day        | repeatable == 1  = addDays 1 day
                                | repeatable == 2  = addDays 7 day
                                | repeatable == 3  = addGregorianMonthsRollOver 1 day
                                | repeatable == 4  = addGregorianYearsRollOver 1 day

--Function for exporting data
exportData world = do
  filePath <- getName name "Where do you want to save the file?"
  if  ( isNothing filePath )
          then do
                putStrLn "No file path!"
                putStrLn ("\n")
                return world
          else do
                exportToFile world (fromJust filePath)
                putStrLn "File saved"
                return world
                                 

--Function for importing data
importData world = do
      filePath <- getName name "What file do you want to load?"
      if isNothing filePath
          then do
                putStrLn "No file path!"
                putStrLn ("\n")
                return world
          else do
                maybeWorld <- importFromFile (fromJust filePath) readmaybeWorld
                if isNothing maybeWorld
                    then do
                      putStrLn "Error: file not found!"
                      putStrLn ("\n")
                      return world
                    else do
                      putStrLn "File loaded successfully!"
                      let newWorld = fromJust maybeWorld
                      return newWorld
                           where
                               readmaybeWorld :: String -> Maybe World
                               readmaybeWorld = readMaybe


