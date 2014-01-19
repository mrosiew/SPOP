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

mainMenu::World -> IO World
mainMenu world = do
        pickedTask <- menu [("Print unfinished tasks", viewAllTasks),
                        ("Print tasks to do today",majtki),
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
                        ("Remove task",majtki),
                         ("Go back to menu",majtki)]
        kek <- pickedTask world
        return kek

--filler
majtki::World -> IO World
majtki world = do
            putStrLn ":DDD"
            return world

            


exit _ = exitWith ExitSuccess
