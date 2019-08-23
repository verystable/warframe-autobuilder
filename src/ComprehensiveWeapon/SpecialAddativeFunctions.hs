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
import           Types.GeneralTypes
import           Types.GenericWeapon

saImpact :: Build -> Maybe Float
saImpact (gw, mods) = Just 0

saPuncture :: Build -> Maybe Float
saPuncture (gw, mods) = Just 0

saSlash :: Build -> Maybe Float
saSlash (gw, mods)
  | "Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]"
    `elem` mods
  = (*) <$> Just 0.3 <*> gw ^. gwCriticalChance
  | otherwise
  = Just 0

saHeat :: Build -> Maybe Float
saHeat (gw, mods) = Just 0

saCold :: Build -> Maybe Float
saCold (gw, mods) = Just 0

saToxin :: Build -> Maybe Float
saToxin (gw, mods) = Just 0

saElectricity :: Build -> Maybe Float
saElectricity (gw, mods) = Just 0

saBlast :: Build -> Maybe Float
saBlast (gw, mods) = Just 0

saGas :: Build -> Maybe Float
saGas (gw, mods) = Just 0

saRadiation :: Build -> Maybe Float
saRadiation (gw, mods) = Just 0

saViral :: Build -> Maybe Float
saViral (gw, mods) = Just 0

saCorrosive :: Build -> Maybe Float
saCorrosive (gw, mods) = Just 0

saMagnetic :: Build -> Maybe Float
saMagnetic (gw, mods) = Just 0
