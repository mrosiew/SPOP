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


import Interface
import Etc

main = do
      showMessageBox welcomeInfo
      mainLoop emptyAddressBook

--Główna pętla
mainLoop addressBook = do
        printNewLine
        function <- showMainMenu
        addressBook <- function addressBook
        mainLoop addressBook

showMainMenu = do
         showMenuBox [("Zarządzanie kontaktami", showSubmenuContactsLoop),
            ("Zarządzanie grupami", showSubmenuGroupsLoop),
            ("Wyszukiwanie kontaktów",showSearchSubmenuLoop),
            ("Wyświetlenie osób obchodzących dzisiaj urodziny",showPersonsBirthdayAction),
            ("Pokaż wszystkie dane",  showAddressBookAction),
            ("Zapis danych do pliku",  saveData),
            ("Odczyt danych z pliku", loadData),
            ("Wyczyszczenie danych", createEmptyAddressBook),
            ("Zakończ", exit)]


#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

