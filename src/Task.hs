-----------------------------------------------------------------------------
--
-- Module      :  Task
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

module Task (
newWorld
) where

import Data.Time
import Data.Maybe
import Data.Char
import Numeric


newWorld = World [] "Poniedzialek"

data World = World {
    tasks::[Task],
    currentDay::String --do implementacji jako dzien
    }

data Task = Task {
    id::Int,
    when::String, --do implementacji jako dzien
    repeatable::Int, --powinien byc jakis smieszny enum
    name::String,
    description::String
} deriving (Show, Read, Eq)

