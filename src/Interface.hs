-----------------------------------------------------------------------------
--
-- Module      :  Interface
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

module Interface where

import Etc
import Data.String.Utils

import Numeric
import Task

--menu::[(String, (a->IO a))] -> IO (a->IO a)
--Executes function that prints menu and returns pickTask
menu list = do
    printMenu textList
    sth <- pickTask taskList
    return sth
    where
        textList = map fst list
        taskList = map snd list

--Prints menu
printMenu::[[Char]]->IO()
printMenu items = do
        printMenuHelper items 1

--Prints menu (helper function)        
printMenuHelper:: [[Char]] -> Integer -> IO()
printMenuHelper (firstItem:menuItems) i = do
        putStrLn((show i) ++ ") " ++ firstItem)
        printMenuHelper menuItems (i+1)
printMenuHelper [] i = do
       return()

--pickTask::[a->IO a]-> IO ( a -> IO c )
--Returns chose fucntion
pickTask items = do
        putStrLn "What do you want to do now?"
        taskNumber <- getInt 0
        let actualTaskNumber = taskNumber - 1
        let listLength = length items
        if actualTaskNumber >= 0 && actualTaskNumber < listLength
            then return (items !! actualTaskNumber)
            else  return wrongNumber

--Function used if wrong number (oprion) was chose by the user
wrongNumber world = do
    putStrLn "Wrong task number"
    return world

--Returns integer passed from user input
getInt defaultValue = do
    maybeInt <- getMaybeInt
    let value = getMaybeIntHelper maybeInt defaultValue
    return value

--Gets (maybe) integer from user input
getMaybeInt = do
        input <- getString
        let smalec = readDec input
        return smalec

--Returns value or dafault value if value is empty        
getMaybeIntHelper [] def = def
getMaybeIntHelper [(id, _)] def = id

--Gets string from input
getString = do
        input <- getLine
        let strippedInput = strip input
        if null strippedInput
            then getString
            else return strippedInput





