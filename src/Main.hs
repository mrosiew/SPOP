{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where


import Menus
import Task

main = do
        mainMenu newWorld

        
