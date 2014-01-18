-----------------------------------------------------------------------------
--
-- Module      :  Interface
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

module Interface where

import Etc
import Data.String.Utils

import Numeric
import Task

menu::[(String, (World->IO World))] -> IO (World->IO World)
menu list = do
    printMenu textList
    sth <- pickTask taskList
    return sth
    where
        textList = map fst list
        taskList = map snd list

printMenu::[[Char]]->IO()
printMenu items = do
        printMenuHelper items 1
printMenuHelper:: [[Char]] -> Integer -> IO()
printMenuHelper (firstItem:menuItems) i = do
        putStrLn((show i) ++ ") " ++ firstItem)
        printMenuHelper menuItems (i+1)
printMenuHelper [] i = do
       return()

pickTask::[World->IO World]-> IO ( World -> IO World )
pickTask items = do
        putStrLn "What do you want to do now?"
        taskNumber <- getInt 0

        let actualTaskNumber = taskNumber - 1
        let listLength = length items
        if actualTaskNumber >= 0 && actualTaskNumber < listLength
            then return (items !! actualTaskNumber)
            else  return wrongNumber

wrongNumber world = do
    putStrLn "Wrong task number"
    return world

getInt defaultValue = do
    maybeInt <- getMaybeInt
    let value = getMaybeIntHelper maybeInt defaultValue
    return value

getMaybeInt = do
        input <- getString
        let smalec = readDec input
        return smalec
getMaybeIntHelper [] def = def
getMaybeIntHelper [(id, _)] def = id

getString = do
        input <- getLine
        let strippedInput = strip input
        if null strippedInput
            then getString
            else return strippedInput





