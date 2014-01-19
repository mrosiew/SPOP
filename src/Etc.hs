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

printNewLine = putStrLn ""
printSeparator = print "---"
printList list = mapM_ putStrLn list


printTask (Task id when repeatable name description isDone) = do
            putStrLn ("id: " ++ (show id))
            putStrLn ("Due date: " ++ (show when))
            putStrLn ("Repeatable: " ++ (getRepe repeatable))
            putStrLn ("Name " ++ name)
            putStrLn ("Description: " ++ description)
            putStrLn ("It's done? " ++ (show isDone) )
            putStrLn ("\n")

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
        case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe


inputBox msg = msg ++ ": "
showPrompt msg = do
        putStrLn (inputBox msg)
        input <- getLine
        let strippedInput = strip input
        if null strippedInput
            then
               showPrompt msg
            else
                return strippedInput

getName objectName msg = do
    objectName <- showPrompt msg
    return (Just objectName)


matchDate :: String  -> [String]
matchDate str = getAllTextMatches $ str =~ "[0-9]+" :: [String]


parseDate :: [String] -> Maybe Day
parseDate (y:m:d:_) = Just (fromGregorian (toInteger (read y ::Int)) (read m ::Int) (read d ::Int))
parseDate _ = Nothing


validateMatchedDate :: [String] -> Day -> Bool
validateMatchedDate (y:m:d:_) date = (length y == 4) && (length m == 2) && (length d == 2) && (y1 == y2) && (m1 == m2) && (d1 == d2)
                                    where (y1,m1,d1) = (toInteger (read y ::Int), read m ::Int, read d ::Int)
                                          (y2,m2,d2) = toGregorian date
validateMatchedDate _ _ = False


getDateWithValidation:: String -> Maybe Day
getDateWithValidation str = if ( not( validateMatchedDate matchedDate ( fromJust maybeDate )) )
                then
                    Nothing
                else
                    maybeDate
                        where
                            matchedDate = matchDate str
                            maybeDate = parseDate (matchedDate);


errStr msg = "Error: " ++ msg
showError msg = putStrLn (errStr msg)


getRepeValidation str | str == "no" = 0
                      | str == "daily" = 1
                      | str == "weekly" = 2
                      | str == "monthly" = 3
                      | otherwise = 5

getRepe int | int == 0  = "no"
            | int == 1  = "daily"
            | int == 2  = "weekly"
            | int == 3  = "monthly"


getTaskById:: Int -> [Task] -> Maybe Task
getTaskById id [] = Nothing
getTaskById id (x:xs) = if (id == getTaskId x )
                            then
                                Just x
                            else
                                getTaskById id xs


removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem t (x:xs) | t == x    = xs
                    | otherwise = x : removeItem t xs


replaceTask index newTask (x:xs)  | index == 0   = newTask : xs
                                  | otherwise    = x : replaceTask (index - 1) newTask xs
     
--let maybeWhen = checkWhen (fromJust maybeWhenfiled)
