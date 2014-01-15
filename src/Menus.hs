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

mainMenu world = do
        menu [("Print unfinished tasks", majtki),
                        ("Print tasks to do today",majtki),
                        ("Print task for particular day",majtki),
                         ("Manage tasks",manageTaskMenu),
                         ("Export tasks to file",majtki),
                         ("Import tasks from file",majtki),
                         ("Change a date (debug)", majtki),
                         ("Exit program", exit)] world
        mainMenu world --mozna zrobic to inaczej ( w sensie bez importowania
        --System.Exit, ale jak tak jest to chyba tez dobrze )

manageTaskMenu world = do
        menu [("Add task", majtki),
                        ("Modify task",majtki),
                        ("Remove task",majtki),
                         ("Go back to menu",majtki)] world
        mainMenu world

--filler
majtki world = do
            putStrLn (":DDD ")
            --return()


exit _ = exitWith ExitSuccess

