{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ArgInterface.PistolWeapons
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains all the useable shotgun weapon data
-- from 'warframe-builder-data/Primary_Weapons' folder.

module ArgInterface.ShotgunWeapons where

import           ClassyPrelude

-- | List of available primary weapons. This list has to be modified
--   if a weapon is added to the 'warframe-autobuilder-data/Primary_Weapons' folder.
listOfAvailableShotguns :: [Text]
listOfAvailableShotguns =
  [ "Arca_Plasmor"
  , "Astilla"
  , "Boar"
  , "Boar_Prime"
  , "Convectrix"
  , "Corinth"
  , "Drakgoon"
  , "Exergis"
  , "Hek"
  , "Kohm"
  , "Mk1-Strun"
  , "Phage"
  , "Phantasma"
  , "Sancti_Tigris"
  , "Sobek"
  , "Strun"
  , "Strun_Wraith"
  , "Tigris"
  , "Tigris_Prime"
  , "Vaykor_Hek"
  ]
