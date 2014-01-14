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

