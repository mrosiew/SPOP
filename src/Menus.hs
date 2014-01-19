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

--import System.Console.ANSI
import Interface
import System.Exit
import Logic
import Task
import Data.Time.Calendar

mainMenu::World -> IO World
mainMenu world = do
      --  temp <- toGregorian day
                putStrLn ("Hi! Today is " ++  (show (todaysDate world )))
                pickedTask <- menu [("Print all tasks", viewAllTasks),
                                    ("Print unfinished tasks", viewUnfinishedTasks),
                                    ("Print tasks to do today",viewTasksToDoToday),
                                    ("Print task for particular day",viewTasksToDoOnX),
                                    ("Manage tasks",manageTaskMenu),
                                    ("Export tasks to file",empty), ---------------
                                    ("Import tasks from file",empty), ---------------
                                    ("Change a date (debug)",changeDate),
                                    ("Exit program", exit)]
                updatedWorld <- pickedTask world
                mainMenu updatedWorld
        --mozna zrobic to inaczej ( w sensie bez importowania
        --System.Exit, ale jak tak jest to chyba tez dobrze )

manageTaskMenu::World -> IO World
manageTaskMenu world = do
        pickedTask <- menu [("Add task", addTask),
                            ("Mark task as done", markAsDone),
                            ("Modify task",modifyTask),
                            ("Remove task",removeTask),
                            ("Go back to menu",empty)]
        kek <- pickedTask world
        return kek


--filler
empty::World -> IO World
empty world = do
            return world

            


exit _ = exitWith ExitSuccess
