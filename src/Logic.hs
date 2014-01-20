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

--import System.Console.ANSI
import Data.List
import Data.Maybe
import Task
import Etc
import Task
import Interface
import Data.Time.Calendar

addTask::World -> IO World
addTask (World tasks day) = do
    maybeWhenfield <- getName when "When is this task due? (yyyy-mm-dd)"
    maybeRepeatablefield <- getName repeatable "Is this task repeatable? ('no, 'daily, 'weekly' or 'monthly', 'yearly')"
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

doAddTask::Int->Day->Int->String->String->Bool->[Task]->[Task]
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
    let taskList = filterTasksOnDate tasks day day
    if taskList == [] then do putStrLn ("\n")
                              putStrLn "No tasks to display"
                              putStrLn ("\n")
                              return (World tasks day)
                      else do printTaskList taskList
                              return (World tasks day)

--viewUpdatedTasksToDoOnX::World->IO World
--viewUpdatedTasksToDoOnX world =  viewTasksToDoOnX (updateEverything world)

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


viewUnfinishedTasks::World-> IO World
viewUnfinishedTasks (World tasks day) = do
    printTaskList (filterFinishedTasks tasks False)
    return (World tasks day)


printTaskList (firstTaskInList:restOfTasks) = do
    printTask firstTaskInList
    printTaskList restOfTasks
    return 1
printTaskList [] = do
    return 0


filterTasksOnDate (firstTaskInList:restOfTasks) after before =
        let dayOfTheTask = getTaskWhen firstTaskInList in
                if dayOfTheTask > after ||
                dayOfTheTask < before
                        then filterTasksOnDate restOfTasks after before
                        else firstTaskInList : (filterTasksOnDate restOfTasks after before)
filterTasksOnDate [] _ _ = []


filterFinishedTasks::[Task] -> Bool-> [Task]
filterFinishedTasks (firstTaskInList:restOfTasks) isDone =
    if getTaskIsDone firstTaskInList == isDone
        then firstTaskInList : (filterFinishedTasks restOfTasks isDone)
        else filterFinishedTasks restOfTasks isDone
filterFinishedTasks [] _ = []


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


todaysDate (World tasks day) = day

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
                    --tutaj zamieniam IO (Task,[Task]) na IO [Task] tak jak powinno byc
                    --dzieje sie to ze z jakiegos powodu funkcje w menu musza zwracac dokladnie to
                    --samo co przyjmuja co jest dosc slabe ale nie da sie tego tak latwo obejsc
                    newTaskList >>= (\ x -> liftWorld (return (snd x)) (return day))
                    --liftWorld asd (return day)

--liftWorld :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--liftWorld::IO m => IO [Task] -> IO Day -> IO World
liftWorld  changedTasks day = do
        x <- changedTasks
        y <- day
        return (World x y)

--modifyTaskMenu::Task->[Task]->IO [Task]
modifyTaskMenu task taskList = do
        pickedTask <- menu [("Modify task's due date", modDate),
                            ("Modify how often the task will be repeated", modRepe), --modRepe),
                            ("Modify task's name",modName), --modName),
                            ("Modify task's description", modDesc), --modDesc),
                            ("Go back to menu",emp)]
        kek <- pickedTask (task, taskList) --
        return kek

emp (task, taskList) = do return (task, taskList)

--modDate::Task->[Task]->IO [Task]
--modyfikacja powtarzania daty
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
--modyfikacja powtarzalnosci
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

--modyfikacja nazwy
modName ((Task id when repeatable name description isDone), tasks) = do
    maybeName <- getName name "Name of the task"
    let oldTask = Task id when repeatable name description isDone
    if  ( isNothing maybeName )
          then do
                putStrLn "Wrong name value!"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id when repeatable (fromJust maybeName) description isDone) in
                                return (oldTask, (replaceTask index newTask tasks))
--modyfikacja opisu
modDesc ((Task id when repeatable name description isDone), tasks) = do
    maybeDescription <- getName description "Describe task"
    let oldTask = Task id when repeatable name description isDone
    if  ( isNothing maybeDescription )
          then do
                putStrLn "Wrong description value!"
                putStrLn ("\n")
                return (oldTask, tasks)
          else do
                putStrLn "Task changed successfully!"
                putStrLn ("\n")
                let index = fromJust (elemIndex oldTask tasks) in
                        let newTask = (Task id when repeatable name (fromJust maybeDescription) isDone) in
                                return (oldTask, (replaceTask index newTask tasks))

updateEverything (World tasks day) =
        return (World (doUpdateEverything(World tasks day) (getNextTaskId tasks)) day)

doUpdateEverything (World [] day) _ = []
doUpdateEverything (World (taskHead:restOfList) day) givenId =
        let updatedList = (updateRepeatable taskHead day givenId) in --fiz w przypadku gdy nie ma nowych id
        updatedList ++ (doUpdateEverything (World restOfList day) (getNextTaskId updatedList))


--propozycja funkcji updatujaca repetujace rzeczy :---D
--modyfikujemy zadanie ustawiajac mu repeatable na 'no'
updateRepeatable (Task id when repeatable name description isDone) day maxId =
        let oldTask = Task id when repeatable name description isDone in
        if repeatable == 0 || when > day
                then [oldTask]
                else
                        let oldUpdatedTask = (Task id when 0 name description isDone) in
                        let newTaskPattern = (Task maxId when repeatable name description False) in
                            oldUpdatedTask:(addRepeatables newTaskPattern day)



addRepeatables (Task givenId when repeatable name description isDone) day  =
        let newDay = getNewDay repeatable when in
        if (newDay <= day) then
                let furtherList = (addRepeatables (Task (givenId+1) newDay repeatable name description False) day) in
                let newTask = (Task givenId newDay 0 name description False) in
                        newTask : furtherList
            else
                let newTask = (Task givenId newDay repeatable name description False) in
                        [newTask]

getNewDay repeatable day        | repeatable == 1  = addDays 1 day
                                | repeatable == 2  = addDays 7 day
                                | repeatable == 3  = addGregorianMonthsRollOver 1 day
                                | repeatable == 4  = addGregorianYearsRollOver 1 day
