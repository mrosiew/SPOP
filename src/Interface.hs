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

menu list world = do
    printMenu textList
    pickTask taskList world
    where
        textList = map fst list
        taskList = map snd list

printMenu items = do
        printMenuHelper items 1
printMenuHelper:: [[Char]] -> Integer -> IO()
printMenuHelper (firstItem:menuItems) i = do
        putStrLn((show i) ++ ") " ++ firstItem)
        printMenuHelper menuItems (i+1)
printMenuHelper [] i = do
       return()

pickTask items world = do
        putStrLn "What do you want to do now?"
        taskNumber <- getInt 0

        let actualTaskNumber = taskNumber - 1
        let listLength = length items
        if actualTaskNumber >= 0 && actualTaskNumber < listLength
            then (items !! actualTaskNumber) world
            else
                putStrLn "Wrong task number"
                --pickTask items world
                --return()


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





