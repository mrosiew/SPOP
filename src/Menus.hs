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

mainMenu::World -> IO World
mainMenu world = do
      --  temp <- toGregorian day
                --putStrLn ("Hi! Today is " ++ (show day))
                pickedTask <- menu [("Print all tasks", viewAllTasks),
                                ("Print unfinished tasks", viewUnfinishedTasks),
                                ("Print tasks to do today",viewTasksToDoToday),
                                ("Print task for particular day",majtki),
                                 ("Manage tasks",manageTaskMenu),
                                 ("Export tasks to file",majtki),
                                 ("Import tasks from file",majtki),
                                 ("Change a date (debug)", majtki),
                                 ("Exit program", exit)]
                updatedWorld <- pickedTask world
                mainMenu updatedWorld
        --mozna zrobic to inaczej ( w sensie bez importowania
        --System.Exit, ale jak tak jest to chyba tez dobrze )

manageTaskMenu::World -> IO World
manageTaskMenu world = do
        pickedTask <- menu [("Add task", addTask),
                        ("Modify task",majtki),
                        ("Remove task",removeTask),
                         ("Go back to menu",majtki)]
        kek <- pickedTask world
        return kek

--filler
majtki::World -> IO World
majtki world = do
            putStrLn ":DDD"
            return world

            


exit _ = exitWith ExitSuccess
