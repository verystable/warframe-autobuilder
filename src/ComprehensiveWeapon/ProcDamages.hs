{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : ComprehensiveWeapon.ProcDamages
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains functions that calculate the proc damages.
-- Each damage type will take a build and return a proc's damage
-- delt by the weapon with that build.

module ComprehensiveWeapon.ProcDamages where

import           ClassyPrelude

import           ComprehensiveWeapon.ModMultiplierFunctions.ColdModMultiplier
import           ComprehensiveWeapon.ModMultiplierFunctions.ElectricityModMultiplier
import           ComprehensiveWeapon.ModMultiplierFunctions.HeatModMultiplier
import           ComprehensiveWeapon.ModMultiplierFunctions.ToxinModMultiplier

import           Control.Lens

import           GenericFunctions.GenericFunctions

import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

getInnateProcDamage
  :: (GenericWeapon, Mods)
  -> Lens' GenericDamage (Maybe Float)
  -> Maybe Float
  -> Maybe Float
getInnateProcDamage (gw, _) property modMultiplier
  | modMultiplier > Just 0 = modMultiplier
  | getGenericDamageProperty gw property > Just 0 = Just 1
  | otherwise              = Just 0

getImpactProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getImpactProcDamage (gw, mods) = Nothing
{-# INLINE getImpactProcDamage #-}

getPunctureProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getPunctureProcDamage (gw, mods) = Nothing
{-# INLINE getPunctureProcDamage #-}

-- | Slash Proc Damage
--   35% of Weapon's base damage over 7 ticks / 6 secs
getSlashProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getSlashProcDamage (gw, mods) =
  (*) <$> Just 7 <*> ((*) <$> Just 0.35 <*> gw ^. gwBaseDamage)
{-# INLINE getSlashProcDamage #-}

getHeatProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getHeatProcDamage (gw, mods) =
  (*)
    <$> Just 7
    <*> (   (*)
        <$> gw
        ^.  gwBaseDamage
        <*> (   (/)
            <$> getInnateProcDamage (gw, mods)
                                    gdHeat
                                    (heatModMultiplier mods)
            <*> Just 2
            )
        )
{-# INLINE getHeatProcDamage #-}

getColdProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getColdProcDamage (gw, mods) = Nothing
{-# INLINE getColdProcDamage #-}

getToxinProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getToxinProcDamage (gw, mods) =
  (*)
    <$> Just 9
    <*> (   (*)
        <$> gw
        ^.  gwBaseDamage
        <*> (   (/)
            <$> getInnateProcDamage (gw, mods)
                                    gdToxin
                                    (toxinModMultiplier mods)
            <*> Just 2
            )
        )
{-# INLINE getToxinProcDamage #-}

getElectricityProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getElectricityProcDamage (gw, mods) =
  (*)
    <$> gw
    ^.  gwBaseDamage
    <*> (   (/)
        <$> getInnateProcDamage (gw, mods)
                                gdElectricity
                                (electricityModMultiplier mods)
        <*> Just 2
        )
{-# INLINE getElectricityProcDamage #-}

getBlastProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getBlastProcDamage (gw, mods) = Nothing
{-# INLINE getBlastProcDamage #-}

getGasProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getGasProcDamage (gw, mods) =
  (+)
    <$> (   (*)
        <$> gw
        ^.  gwBaseDamage
        <*> (   (/)
            <$> getInnateProcDamage (gw, mods)
                                    gdGas
                                    (toxinModMultiplier mods)
            <*> Just 2
            )
        )
    <*> (   (*)
        <$> Just 9
        <*> (   (*)
            <$> gw
            ^.  gwBaseDamage
            <*> (   (**)
                <$> (   (/)
                    <$> getInnateProcDamage (gw, mods)
                                            gdGas
                                            (toxinModMultiplier mods)
                    <*> Just 2
                    )
                <*> Just 2
                )
            )
        )
{-# INLINE getGasProcDamage #-}

getRadiationProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getRadiationProcDamage (gw, mods) = Nothing
{-# INLINE getRadiationProcDamage #-}

getViralProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getViralProcDamage (gw, mods) = Nothing
{-# INLINE getViralProcDamage #-}

getCorrosiveProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getCorrosiveProcDamage (gw, mods) = Nothing
{-# INLINE getCorrosiveProcDamage #-}

getMagneticProcDamage :: (GenericWeapon, Mods) -> Maybe Float
getMagneticProcDamage (gw, mods) = Nothing
{-# INLINE getMagneticProcDamage #-}
