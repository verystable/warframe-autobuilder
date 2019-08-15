{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.SpecialAddativeFunctions
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module deals with special effects any mod may have on a damage type's proc chance.
-- e.g. Hunter Munitions forces slash for 30% of the weapon's critical chance.

module ComprehensiveWeapon.SpecialAddativeFunctions where

import           ClassyPrelude
import           Control.Lens
import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

saImpact :: (GenericWeapon, Mods) -> Maybe Float
saImpact (gw, mods) = Just 0

saPuncture :: (GenericWeapon, Mods) -> Maybe Float
saPuncture (gw, mods) = Just 0

saSlash :: (GenericWeapon, Mods) -> Maybe Float
saSlash (gw, mods)
  | "Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]"
    `elem` mods
  = (*) <$> Just 0.3 <*> gw ^. gwCriticalChance
  | otherwise
  = Just 0

saHeat :: (GenericWeapon, Mods) -> Maybe Float
saHeat (gw, mods) = Just 0

saCold :: (GenericWeapon, Mods) -> Maybe Float
saCold (gw, mods) = Just 0

saToxin :: (GenericWeapon, Mods) -> Maybe Float
saToxin (gw, mods) = Just 0

saElectricity :: (GenericWeapon, Mods) -> Maybe Float
saElectricity (gw, mods) = Just 0

saBlast :: (GenericWeapon, Mods) -> Maybe Float
saBlast (gw, mods) = Just 0

saGas :: (GenericWeapon, Mods) -> Maybe Float
saGas (gw, mods) = Just 0

saRadiation :: (GenericWeapon, Mods) -> Maybe Float
saRadiation (gw, mods) = Just 0

saViral :: (GenericWeapon, Mods) -> Maybe Float
saViral (gw, mods) = Just 0

saCorrosive :: (GenericWeapon, Mods) -> Maybe Float
saCorrosive (gw, mods) = Just 0

saMagnetic :: (GenericWeapon, Mods) -> Maybe Float
saMagnetic (gw, mods) = Just 0
