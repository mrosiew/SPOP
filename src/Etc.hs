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

printNewLine = putStrLn ""
printSeparator = print "---"
printList list = mapM_ putStrLn list

-- Wczytanie obiekt Maybe z ciagu znakow
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
        case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

