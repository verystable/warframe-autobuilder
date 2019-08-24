{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ArgInterface.WeaponDataDirectoryTest
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module tests if data directory exists.

module ArgInterface.WeaponDataDirectoryTest
  ( checkIfDirectoryExists
  )
where

import           ClassyPrelude
import           System.Directory

-- | Error message that'll pop up if autobuilder couldn't find data files.
missingDirectoryMessage :: IO ()
missingDirectoryMessage = do
  putStrLn "Please copy directory"
  putStrLn "'<cloned repo location>/warframe-autobuilder-data'"
  putStrLn "to"
  putStrLn "'.config/warframe-autobuilder-data'"
  putStrLn "---------------------------------------------------"

-- | Checks weather data files directory exists.
checkIfDirectoryExists :: IO ()
checkIfDirectoryExists = do
  homeDir <- getHomeDirectory

  let dataDirectoryPath = homeDir ++ "/.config/warframe-autobuilder-data"

  doesDataDirectoryExists <- doesDirectoryExist dataDirectoryPath

  if doesDataDirectoryExists
    then do
      dirContents <- listDirectory dataDirectoryPath
      when (null dirContents) missingDirectoryMessage
    else do
      createDirectoryIfMissing True dataDirectoryPath
      missingDirectoryMessage
