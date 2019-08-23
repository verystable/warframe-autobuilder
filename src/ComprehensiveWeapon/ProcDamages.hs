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

import           Types.GeneralTypes
import           Types.GenericWeapon

getInnateProcDamage
  :: Build
  -> Lens' GenericDamage (Maybe Float)
  -> Maybe Float
  -> Maybe Float
getInnateProcDamage (gw, _) property modMultiplier
  | modMultiplier > Just 0 = modMultiplier
  | getGenericDamageProperty gw property > Just 0 = Just 1
  | otherwise              = Just 0

getImpactProcDamage :: Build -> Maybe Float
getImpactProcDamage (gw, mods) = Nothing
{-# INLINE getImpactProcDamage #-}

getPunctureProcDamage :: Build -> Maybe Float
getPunctureProcDamage (gw, mods) = Nothing
{-# INLINE getPunctureProcDamage #-}

-- | Slash Proc Damage
--   35% of Weapon's base damage over 7 ticks / 6 secs
getSlashProcDamage :: Build -> Maybe Float
getSlashProcDamage (gw, mods) =
  (*) <$> Just 7 <*> ((*) <$> Just 0.35 <*> gw ^. gwBaseDamage)
{-# INLINE getSlashProcDamage #-}

getHeatProcDamage :: Build -> Maybe Float
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

getColdProcDamage :: Build -> Maybe Float
getColdProcDamage (gw, mods) = Nothing
{-# INLINE getColdProcDamage #-}

getToxinProcDamage :: Build -> Maybe Float
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

getElectricityProcDamage :: Build -> Maybe Float
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

getBlastProcDamage :: Build -> Maybe Float
getBlastProcDamage (gw, mods) = Nothing
{-# INLINE getBlastProcDamage #-}

getGasProcDamage :: Build -> Maybe Float
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

getRadiationProcDamage :: Build -> Maybe Float
getRadiationProcDamage (gw, mods) = Nothing
{-# INLINE getRadiationProcDamage #-}

getViralProcDamage :: Build -> Maybe Float
getViralProcDamage (gw, mods) = Nothing
{-# INLINE getViralProcDamage #-}

getCorrosiveProcDamage :: Build -> Maybe Float
getCorrosiveProcDamage (gw, mods) = Nothing
{-# INLINE getCorrosiveProcDamage #-}

getMagneticProcDamage :: Build -> Maybe Float
getMagneticProcDamage (gw, mods) = Nothing
{-# INLINE getMagneticProcDamage #-}
