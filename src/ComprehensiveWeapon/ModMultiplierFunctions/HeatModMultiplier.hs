{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ModMultiplierFunctions.HeatModMultiplier
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains 'heatModMultiplier' that returns heat damage's mod multiplier
-- on a generic weapon build.

module ComprehensiveWeapon.ModMultiplierFunctions.HeatModMultiplier where

import           ClassyPrelude
import           Types.GeneralTypes

-- | Finds the electricity multiplier of a rifle build
heatModMultiplierRifles :: Mods -> Maybe Float
heatModMultiplierRifles mods
  | "Primed Hellfire [+165% Heat Damage]"
    `elem` mods
    &&     "Thermite Rounds [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Wildfire [+20% Magazine Size, +60% Heat Damage]"
    `elem` mods
  = Just 2.85
  | "Hellfire [+90% Heat Damage]"
    `elem` mods
    &&     "Thermite Rounds [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Wildfire [+20% Magazine Size, +60% Heat Damage]"
    `elem` mods
  = Just 2.1
  | "Primed Hellfire [+165% Heat Damage]"
    `elem` mods
    &&     "Thermite Rounds [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Hellfire [+165% Heat Damage]"
    `elem` mods
    &&     "Wildfire [+20% Magazine Size, +60% Heat Damage]"
    `elem` mods
  = Just 2.25
  | "Hellfire [+90% Heat Damage]"
    `elem` mods
    &&     "Thermite Rounds [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Hellfire [+90% Heat Damage]"
    `elem` mods
    &&     "Wildfire [+20% Magazine Size, +60% Heat Damage]"
    `elem` mods
  = Just 1.5
  | "Thermite Rounds [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Wildfire [+20% Magazine Size, +60% Heat Damage]"
    `elem` mods
  = Just 1.2
  | "Primed Hellfire [+165% Heat Damage]" `elem` mods
  = Just 1.65
  | "Hellfire [+90% Heat Damage]" `elem` mods
  = Just 0.9
  | "Thermite Rounds [+60% Heat Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Wildfire [+20% Magazine Size, +60% Heat Damage]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the electricity multiplier of a shotgun build
heatModMultiplierShotguns :: Mods -> Maybe Float
heatModMultiplierShotguns mods
  | "Primed Incendiary Coat [+165% Heat Damage]"
    `elem` mods
    &&     "Scattering Inferno [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Blaze [+60% Damage, +60% Heat Daage]"
    `elem` mods
  = Just 2.85
  | "Incendiary Coat [+90% Heat Damage]"
    `elem` mods
    &&     "Scattering Inferno [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Blaze [+60% Damage, +60% Heat Daage]"
    `elem` mods
  = Just 2.1
  | "Primed Incendiary Coat [+165% Heat Damage]"
    `elem` mods
    &&     "Scattering Inferno [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Incendiary Coat [+165% Heat Damage]"
    `elem` mods
    &&     "Blaze [+60% Damage, +60% Heat Daage]"
    `elem` mods
  = Just 2.25
  | "Incendiary Coat [+90% Heat Damage]"
    `elem` mods
    &&     "Scattering Inferno [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Incendiary Coat [+90% Heat Damage]"
    `elem` mods
    &&     "Blaze [+60% Damage, +60% Heat Daage]"
    `elem` mods
  = Just 1.5
  | "Scattering Inferno [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
    &&     "Blaze [+60% Damage, +60% Heat Daage]"
    `elem` mods
  = Just 1.2
  | "Primed Incendiary Coat [+165% Heat Damage]" `elem` mods
  = Just 1.65
  | "Incendiary Coat [+90% Heat Damage]" `elem` mods
  = Just 0.9
  | "Scattering Inferno [+60% Heat Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Blaze [+60% Damage, +60% Heat Daage]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the electricity multiplier of a pistol build
heatModMultiplierPistols :: Mods -> Maybe Float
heatModMultiplierPistols mods
  | "Primed Heated Charge [+165% Heat Damage]"
    `elem` mods
    &&     "Scorch [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Heated Charge [+90% Heat Damage]"
    `elem` mods
    &&     "Scorch [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Heated Charge [+165% Heat Damage]" `elem` mods
  = Just 1.65
  | "Heated Charge [+90% Heat Damage]" `elem` mods
  = Just 0.9
  | "Scorch [+60% Heat Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the electricity multiplier of a melee build
heatModMultiplierMelees :: Mods -> Maybe Float
heatModMultiplierMelees mods
  | "Primed Molten Impact [+165% Heat Damage]"
    `elem` mods
    &&     "Volcanic Edge [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Molten Impact [+90% Heat Damage]"
    `elem` mods
    &&     "Volcanic Edge [+60% Heat Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Molten Impact [+165% Heat Damage]" `elem` mods
  = Just 1.65
  | "Molten Impact [+90% Heat Damage]" `elem` mods
  = Just 0.9
  | "Volcanic Edge [+60% Heat Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the electricity multiplier of a general build
heatModMultiplier :: Mods -> Maybe Float
heatModMultiplier mods =
  heatModMultiplierRifles mods
    <|> heatModMultiplierShotguns mods
    <|> heatModMultiplierPistols mods
    <|> heatModMultiplierMelees mods
