{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : ModMaker.ModMaker
-- Maintainer  : verystable@proton.com
-- Stability   : Work in progress
--
-- This module contains functions that parse userinput into a mod.

module ModMaker.ModMaker where

import           ClassyPrelude

import           Types.GeneralTypes

-- | parsePotentialMod parses given user input into a PotentialMod type
--   Weapon Category, Weapon Type, Weapon Name, Mod Name, [Mod Stats]
--   Sample user input
--   Primary Weapon, Rifle, Example Rifle, Example Mod, [+90% Magazine Size, +60% Heat Damage]
parsePotentialMod :: Text -> PotentialMod
parsePotentialMod userinput = PotentialMod
  { weaponCategory = weaponCategory'
  , weaponType     = weaponType'
  , weaponName     = weaponName'
  , modName        = modName'
  , modStats       = modStats'
  }
 where
  splitArg        = splitWhen (== ',') userinput

  weaponCategory' = headEx splitArg
  weaponType'     = headEx . tailEx $ splitArg
  weaponName'     = headEx . tailEx . tailEx $ splitArg :: Text
  modName'        = headEx . tailEx . tailEx . tailEx $ splitArg :: Text
  modStats'       = dropWhile (/= '[') userinput :: Text
