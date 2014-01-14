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

import Data.String.Utils

import Etc


showMessageBox message = putStr (messageBox message)

messageBox "" = ""
messageBox message =
        unlines (surround centeredMessageLinesWithBars verticalBar)
        where
                messageLines = map (surroundWith ' ') (lines message)
                messageLengths = map length messageLines
                maximumMessageLength = maximum messageLengths
                verticalBar = surround (take maximumMessageLength (repeat '-')) ' '
                centeredMessageLines = map (center maximumMessageLength ' ') messageLines
                centeredMessageLinesWithBars = map (surroundWith '|') centeredMessageLines
                surroundWith item = flip (surround) item
