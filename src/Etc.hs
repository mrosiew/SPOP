-----------------------------------------------------------------------------
--
-- Module      :  Etc
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

module Etc where

import Data.Time
import Data.String.Utils
import Numeric
import Text.Regex.Posix
import Data.Maybe
import Task
import Control.Exception
import Data.List

--Prints task data
printTask (Task id when repeatable name description isDone) = do
            putStrLn ("id: " ++ (show id))
            putStrLn ("Due date: " ++ (show when))
            putStrLn ("Repeatable: " ++ (getRepe repeatable))
            putStrLn ("Name " ++ name)
            putStrLn ("Description: " ++ description)
            putStrLn ("It's done? " ++ (show isDone) )
            putStrLn ("\n")

--Returns value or Nothing if value is empty
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
        case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

--Prompts the user to enter sting 
showPrompt msg = do
        putStrLn (inputBox msg)
        input <- getLine
        let strippedInput = strip input
        if null strippedInput
            then
               showPrompt msg
            else
                return strippedInput
inputBox msg = msg ++ ": "

--Gest string from input
getName objectName msg = do
    objectName <- showPrompt msg
    return (Just objectName)

--Matches date using regular expression
matchDate :: String  -> [String]
matchDate str = getAllTextMatches $ str =~ "[0-9]+" :: [String]

--Parses date
parseDate :: [String] -> Maybe Day
parseDate (y:m:d:_) = Just (fromGregorian (toInteger (read y ::Int)) (read m ::Int) (read d ::Int))
parseDate _ = Nothing

--Validates date
validateMatchedDate :: [String] -> Day -> Bool
validateMatchedDate (y:m:d:_) date = (length y == 4) && (length m == 2) && (length d == 2) && (y1 == y2) && (m1 == m2) && (d1 == d2)
                                    where (y1,m1,d1) = (toInteger (read y ::Int), read m ::Int, read d ::Int)
                                          (y2,m2,d2) = toGregorian date
validateMatchedDate _ _ = False

--Returns date as string
getDateWithValidation:: String -> Maybe Day
getDateWithValidation str = if ( not( validateMatchedDate matchedDate ( fromJust maybeDate )) )
                then
                    Nothing
                else
                    maybeDate
                        where
                            matchedDate = matchDate str
                            maybeDate = parseDate (matchedDate);

--Changes repeatable sting to integer value (to store it)
getRepeValidation str | str == "no" = 0
                      | str == "daily" = 1
                      | str == "weekly" = 2
                      | str == "monthly" = 3
                      | str == "yearly" = 4
                      | otherwise = 5

--Changes repeatable int value to string (to display it)
getRepe int | int == 0  = "no"
            | int == 1  = "daily"
            | int == 2  = "weekly"
            | int == 3  = "monthly"
            | int == 4  = "yearly"

--Returns a task with specific id
getTaskById:: Int -> [Task] -> Maybe Task
getTaskById id [] = Nothing
getTaskById id (x:xs) = if (id == getTaskId x )
                            then
                                Just x
                            else
                                getTaskById id xs

--Removes a task from tasks list
removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem t (x:xs) | t == x    = xs
                    | otherwise = x : removeItem t xs

--Replaces a task in tasks list
replaceTask::Int->Task->[Task]->[Task]
replaceTask index newTask (x:xs)  | index == 0   = newTask : xs
                                  | otherwise    = x : replaceTask (index - 1) newTask xs
     
--Saves world to a file
exportToFile :: (Show a) => a -> FilePath -> IO ()
exportToFile expWorld filePath = writeFile filePath (show expWorld)

--Loads world from a file
importFromFile :: FilePath -> (String -> b) -> IO b
importFromFile filePath impWorld = do
        raw <- readFile filePath
        return (impWorld (strip raw))

{-
--Funtion used for parsing hour
parseHour :: [[Char]] -> Int -> [Char]
--parseHour [] _ = ""
parseHour (x:xs) a = do
    let num = read x :: Integer
    if a == 2
        then if num >= 0 && num <= 23
                then return (x ++ (parseHour xs 1))
                else return ""
        else if a == 1
                then if num >= 0 && num <= 59
                        then if num == 0
                                then return "00"
                                else return x
                        else return "00"
        else return ""
-}



