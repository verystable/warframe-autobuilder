{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ProcChances
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains the function that calculate proc chances for
-- each of the damage type a weapon may have.
-- Proc Chance is based on the percentage of damage each damage type has
-- and the total status chance.

module ComprehensiveWeapon.ProcChances where

import           ClassyPrelude
import           Control.Lens
import           Types.GenericWeapon

totalDamageWeightage :: GenericDamage -> Float
totalDamageWeightage wep = sum $ catMaybes
  [ (*) <$> Just 4 <*> wep ^. gdImpact
  , (*) <$> Just 4 <*> wep ^. gdPuncture
  , (*) <$> Just 4 <*> wep ^. gdSlash
  , wep ^. gdHeat
  , wep ^. gdCold
  , wep ^. gdToxin
  , wep ^. gdElectricity
  , wep ^. gdBlast
  , wep ^. gdGas
  , wep ^. gdRadiation
  , wep ^. gdViral
  , wep ^. gdCorrosive
  , wep ^. gdMagnetic
  ]

getProcChancePhysical
  :: (Lens' GenericDamage (Maybe Float)) -> GenericWeapon -> Maybe Float
getProcChancePhysical damage wep =
  (*)
    <$> totalStatusChance
    <*> ((/) <$> ((*) <$> Just 4 <*> baseStat) <*> totalDamageWeightage')
 where
  baseStat = fromMaybe (Just 0) $ wep ^. gwDamageTypes & _Just %~ view damage
  totalDamageWeightage' =
    totalDamageWeightage <$> wep ^. gwDamageTypes :: Maybe Float
  totalStatusChance = wep ^. gwStatusChance

getProcChanceElemental
  :: (Lens' GenericDamage (Maybe Float)) -> GenericWeapon -> Maybe Float
getProcChanceElemental damage wep =
  (*)
    <$> totalStatusChance
    <*> ((/) <$> ((*) <$> Just 1 <*> baseStat) <*> totalDamageWeightage')
 where
  baseStat = fromMaybe (Just 0) $ wep ^. gwDamageTypes & _Just %~ view damage
  totalDamageWeightage' =
    totalDamageWeightage <$> wep ^. gwDamageTypes :: Maybe Float
  totalStatusChance = wep ^. gwStatusChance
