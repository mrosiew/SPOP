-----------------------------------------------------------------------------
--
-- Module      :  Menus
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

module Menus (mainMenu) where

import Interface
import System.Exit
import Logic
import Task
import Data.Time.Calendar

--Program's main loop
mainMenu::World -> IO World
mainMenu world = do
                putStrLn ("Hi! Today is " ++  (show (todaysDate world )))
                pickedTask <- menu [("Print all tasks", viewAllTasks),
                                    ("Print unfinished tasks", viewUnfinishedTasks),
                                    ("Print tasks to do today",viewUpdatedTasksToDoToday),
                                    ("Print task for particular day",viewUpdatedTasksToDoOnX),
                                    ("Manage tasks",manageTaskMenu),
                                    ("Export tasks to file",exportData),
                                    ("Import tasks from file",importData),
                                    ("Change a date (debug)",changeDate),
                                    ("Exit program", exit)]
                updatedWorld <- pickedTask world
                mainMenu updatedWorld

--Defines manage task submenu
manageTaskMenu::World -> IO World
manageTaskMenu world = do
        pickedTask <- menu [("Add task", addTask),
                            ("Mark task as done", markAsDone),
                            ("Modify task",modifyTask),
                            ("Remove task",removeTask),
                            ("Go back to menu",empty)]
        kek <- pickedTask world
        return kek


--Filler function
empty::World -> IO World
empty world = do
            return world

--Exits program            
exit _ = exitWith ExitSuccess
