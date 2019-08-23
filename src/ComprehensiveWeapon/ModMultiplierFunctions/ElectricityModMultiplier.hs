{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ModMultiplierFunctions.ElectricityModMultiplier
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains 'electricityModMultiplier' that returns electricity damage's mod multiplier
-- on a generic weapon build.

module ComprehensiveWeapon.ModMultiplierFunctions.ElectricityModMultiplier
where

import           ClassyPrelude
import           Types.GeneralTypes

electricityModMultiplierRifles :: Mods -> Maybe Float
electricityModMultiplierRifles mods
  | "Primed Stormbringer [+165% Electricity Damage]"
    `elem` mods
    &&     "High Voltage [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Stormbringer [+90% Electricity Damage]"
    `elem` mods
    &&     "High Voltage [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Stormbringer [+165% Electricity Damage]" `elem` mods
  = Just 1.65
  | "Stormbringer [+90% Electricity Damage]" `elem` mods
  = Just 0.9
  | "High Voltage [+60% Electricity Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

electricityModMultiplierShotguns :: Mods -> Maybe Float
electricityModMultiplierShotguns mods
  | "Primed Charged Shell [+165% Electricity Damage]"
    `elem` mods
    &&     "Shell Shock [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Charged Shell [+90% Electricity Damage]"
    `elem` mods
    &&     "Shell Shock [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Charged Shell [+165% Electricity Damage]" `elem` mods
  = Just 1.65
  | "Charged Shell [+90% Electricity Damage]" `elem` mods
  = Just 0.9
  | "Shell Shock [+60% Electricity Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

electricityModMultiplierPistols :: Mods -> Maybe Float
electricityModMultiplierPistols mods
  | "Primed Convulsion [+165% Electricity Damage]"
    `elem` mods
    &&     "Jolt [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Convulsion [+90% Electricity Damage]"
    `elem` mods
    &&     "Jolt [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Convulsion [+165% Electricity Damage]" `elem` mods
  = Just 1.65
  | "Convulsion [+90% Electricity Damage]" `elem` mods
  = Just 0.9
  | "Jolt [+60% Electricity Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

electricityModMultiplierMelees :: Mods -> Maybe Float
electricityModMultiplierMelees mods
  | "Primed Shocking Touch [+165% Electricity Damage]"
    `elem` mods
    &&     "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
    &&     "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 2.85
  | "Shocking Touch [+90% Electricity Damage]"
    `elem` mods
    &&     "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
    &&     "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 2.1
  | "Primed Shocking Touch [+165% Electricity Damage]"
    `elem` mods
    &&     "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Shocking Touch [+165% Electricity Damage]"
    `elem` mods
    &&     "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 2.25
  | "Shocking Touch [+90% Electricity Damage]"
    `elem` mods
    &&     "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Shocking Touch [+90% Electricity Damage]"
    `elem` mods
    &&     "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 1.5
  | "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]"
    `elem` mods
    &&     "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 1.2
  | "Primed Shocking Touch  [+165% Electricity Damage]" `elem` mods
  = Just 1.65
  | "Shocking Touch [+90% Electricity Damage]" `elem` mods
  = Just 0.9
  | "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Focus Energy [+40% Channeling Efficiency, +60% Electricity Damage]"
    `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

electricityModMultiplier :: Mods -> Maybe Float
electricityModMultiplier mods =
  electricityModMultiplierRifles mods
    <|> electricityModMultiplierShotguns mods
    <|> electricityModMultiplierPistols mods
    <|> electricityModMultiplierMelees mods
