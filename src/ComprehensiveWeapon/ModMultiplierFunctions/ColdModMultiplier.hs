{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ModMultiplierFunctions.ColdModMultiplier
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains 'coldModMultiplier' that returns cold damage's mod multiplier
-- on a generic weapon build.

module ComprehensiveWeapon.ModMultiplierFunctions.ColdModMultiplier where

import           ClassyPrelude
import           Types.GeneralTypes

-- | Finds the cold multiplier of a shotgun build
coldModMultiplierShotguns :: Mods -> Maybe Float
coldModMultiplierShotguns mods
  | "Primed Chilling Grasp [+165% Cold Damage]"
    `elem` mods
    &&     "Frigid Blast [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Chilling Reload [+60% Cold Damage, -40% Reload Time]"
    `elem` mods
  = Just 2.85
  | "Chilling Grasp [+90% Cold Damage]"
    `elem` mods
    &&     "Frigid Blast [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Chilling Reload [+60% Cold Damage, -40% Reload Time]"
    `elem` mods
  = Just 2.1
  | "Primed Chilling Grasp [+165% Cold Damage]"
    `elem` mods
    &&     "Frigid Blast [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Chilling Grasp [+165% Cold Damage]"
    `elem` mods
    &&     "Chilling Reload [+60% Cold Damage, -40% Reload Time]"
    `elem` mods
  = Just 2.25
  | "Chilling Grasp [+90% Cold Damage]"
    `elem` mods
    &&     "Frigid Blast [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Chilling Grasp [+90% Cold Damage]"
    `elem` mods
    &&     "Chilling Reload [+60% Cold Damage, -40% Reload Time]"
    `elem` mods
  = Just 1.5
  | "Frigid Blast [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Chilling Reload [+60% Cold Damage, -40% Reload Time]"
    `elem` mods
  = Just 1.2
  | "Primed Chilling Grasp [+165% Cold Damage]" `elem` mods
  = Just 1.65
  | "Chilling Grasp [+90% Cold Damage]" `elem` mods
  = Just 0.9
  | "Frigid Blast [+60% Cold Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Chilling Reload [+60% Cold Damage, -40% Reload Time]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the cold multiplier of a rifle build
coldModMultiplierRifles :: Mods -> Maybe Float
coldModMultiplierRifles mods
  | "Primed Cryo Rounds [+165% Cold Damage]"
    `elem` mods
    &&     "Rime Rounds [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Cryo Rounds [+165% Cold Damage]"
    `elem` mods
    &&     "Rime Rounds [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Cryo Rounds [+165% Cold Damage]" `elem` mods
  = Just 1.65
  | "Cryo Rounds [+165% Cold Damage]" `elem` mods
  = Just 0.9
  | "Rime Rounds [+60% Cold Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the cold multiplier of a pistol build
coldModMultiplierPistols :: Mods -> Maybe Float
coldModMultiplierPistols mods
  | "Primed Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Frostbite [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Ice Storm [+60% Cold Damage, +40% Magazine Size]"
    `elem` mods
  = Just 2.85
  | "Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Frostbite [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Ice Storm [+60% Cold Damage, +40% Magazine Size]"
    `elem` mods
  = Just 2.1
  | "Primed Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Frostbite [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Ice Storm [+60% Cold Damage, +40% Magazine Size]"
    `elem` mods
  = Just 2.25
  | "Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Frostbite [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Deep Freeze [+165% Cold Damage]"
    `elem` mods
    &&     "Ice Storm [+60% Cold Damage, +40% Magazine Size]"
    `elem` mods
  = Just 1.5
  | "Frostbite [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
    &&     "Ice Storm [+60% Cold Damage, +40% Magazine Size]"
    `elem` mods
  = Just 1.2
  | "Primed Deep Freeze [+165% Cold Damage]" `elem` mods
  = Just 1.65
  | "Deep Freeze [+165% Cold Damage]" `elem` mods
  = Just 0.9
  | "Frostbite [+60% Cold Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Ice Storm [+60% Cold Damage, +40% Magazine Size]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the cold multiplier of a melee build
coldModMultiplierMelees :: Mods -> Maybe Float
coldModMultiplierMelees mods
  | "Primed North Wind [+165% Cold Damage]"
    `elem` mods
    &&     "Vicious Frost [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "North Wind [+165% Cold Damage]"
    `elem` mods
    &&     "Vicious Frost [+60% Cold Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed North Wind [+165% Cold Damage]" `elem` mods
  = Just 1.65
  | "North Wind [+165% Cold Damage]" `elem` mods
  = Just 0.9
  | "Vicious Frost [+60% Cold Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the cold multiplier of a general build
coldModMultiplier :: Mods -> Maybe Float
coldModMultiplier mods =
  coldModMultiplierRifles mods
    <|> coldModMultiplierShotguns mods
    <|> coldModMultiplierPistols mods
    <|> coldModMultiplierMelees mods
