{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ModMultiplierFunctions.ToxinModMultiplier
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains 'toxinModMultiplier' that returns toxin damage's mod multiplier
-- on a generic weapon build.

module ComprehensiveWeapon.ModMultiplierFunctions.ToxinModMultiplier where

import           ClassyPrelude
import           Types.GeneralTypes

-- | Finds the toxin multiplier of a rifle build
toxinModMultiplierRifles :: Mods -> Maybe Float
toxinModMultiplierRifles mods
  | "Primed Infected Clip [+165% Toxin Damage]"
    `elem` mods
    &&     "Malignant Force [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Infected Clip [+90% Toxin Damage]"
    `elem` mods
    &&     "Malignant Force [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Infected Clip [+165% Toxin Damage]" `elem` mods
  = Just 1.65
  | "Infected Clip [+90% Toxin Damage]" `elem` mods
  = Just 0.9
  | "Malignant Force [+60% Toxin Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the toxin multiplier of a shotgun build
toxinModMultiplierShotguns :: Mods -> Maybe Float
toxinModMultiplierShotguns mods
  | "Primed Contagious Spread [+165% Toxin Damage]"
    `elem` mods
    &&     "Toxic Barrage [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Contagious Spread [+90% Toxin Damage]"
    `elem` mods
    &&     "Toxic Barrage [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Contagious Spread [+165% Toxin Damage]" `elem` mods
  = Just 1.65
  | "Contagious Spread [+90% Toxin Damage]" `elem` mods
  = Just 0.9
  | "Toxic Barrage [+60% Toxin Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the toxin multiplier of a pistol build
toxinModMultiplierPistols :: Mods -> Maybe Float
toxinModMultiplierPistols mods
  | "Primed Pathogen Rounds [+165% Toxin Damage]"
    `elem` mods
    &&     "Pistol Pestilence [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Pathogen Rounds [+90% Toxin Damage]"
    `elem` mods
    &&     "Pistol Pestilence [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.5
  | "Primed Pathogen Rounds [+165% Toxin Damage]" `elem` mods
  = Just 1.65
  | "Pathogen Rounds [+90% Toxin Damage]" `elem` mods
  = Just 0.9
  | "Pistol Pestilence [+60% Toxin Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | otherwise
  = Nothing

-- | Finds the toxin multiplier of a melee build
toxinModMultiplierMelees :: Mods -> Maybe Float
toxinModMultiplierMelees mods
  | "Primed Fever Strike [+165% Toxin Damage]"
    `elem` mods
    &&     "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
    &&     "Toxic Blight [+100% Toxin Damage, +1 Blight]"
    `elem` mods
  = Just 3.25
  | "Fever Strike [+90% Toxin Damage]"
    `elem` mods
    &&     "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
    &&     "Toxic Blight [+100% Toxin Damage, +1 Blight]"
    `elem` mods
  = Just 2.5
  | "Primed Fever Strike [+165% Toxin Damage]"
    `elem` mods
    &&     "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 2.25
  | "Primed Fever Strike [+165% Toxin Damage]"
    `elem` mods
    &&     "Toxic Blight [+100% Toxin Damage, +1 Blight]"
    `elem` mods
  = Just 2.65
  | "Fever Strike"
    `elem` mods
    &&     "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
  = Just 1.9
  | "Fever Strike"
    `elem` mods
    &&     "Toxic Blight [+100% Toxin Damage, +1 Blight]"
    `elem` mods
  = Just 1.9
  | "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]"
    `elem` mods
    &&     "Toxic Blight [+100% Toxin Damage, +1 Blight]"
    `elem` mods
  = Just 1.6
  | "Primed Fever Strike [+165% Toxin Damage]" `elem` mods
  = Just 1.65
  | "Fever Strike [+90% Toxin Damage]" `elem` mods
  = Just 0.9
  | "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]" `elem` mods
  = Just 0.6
  | "Toxic Blight [+100% Toxin Damage, +1 Blight]" `elem` mods
  = Just 1
  | otherwise
  = Nothing

-- | Finds the toxin multiplier of a general build
toxinModMultiplier :: Mods -> Maybe Float
toxinModMultiplier mods =
  toxinModMultiplierRifles mods
    <|> toxinModMultiplierShotguns mods
    <|> toxinModMultiplierPistols mods
    <|> toxinModMultiplierMelees mods
