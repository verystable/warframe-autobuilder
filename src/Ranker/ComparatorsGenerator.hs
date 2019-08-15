{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Ranker.ComparatorsGenerator
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module converts user input comparator name into
-- an actual comparator type.

module Ranker.ComparatorsGenerator where

import           ClassyPrelude

import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

import           Data.FuzzySet                 as F

import           Ranker.Comparators

betterMatch' :: (Ord a1, Fractional a1) => Maybe (a1, a2) -> Maybe a2
betterMatch' (Just (matchPoints, match)) =
  if matchPoints > 0.85 then Just match else Nothing
betterMatch' _ = Nothing

bestMatch' :: Text -> FuzzySet -> Maybe Text
bestMatch' stuff set = betterMatch' $ headMay $ F.get set stuff
{-# INLINE bestMatch' #-}

bestMatch :: Text -> Maybe Text
bestMatch stuff =
  bestMatch' stuff (F.fromList comparatorIListComprehensive)
    <|> bestMatch' stuff (F.fromList comparatorIListGeneral)
    <|> bestMatch' stuff (F.fromList comparatorIListDamage)
    <|> bestMatch' stuff (F.fromList comparatorIListProcChances)
    <|> bestMatch' stuff (F.fromList comparatorIListProcDPSs)
{-# INLINE bestMatch #-}

comparatorIListGeneral :: [Text]
comparatorIListGeneral =
  [ "MagazineSize"
  , "ReloadTime"
  , "TotalDamage"
  , "BaseDamage"
  , "CriticalChance"
  , "CriticalMultiplier"
  , "StatusChance"
  , "FireRate"
  , "ChargeAttack"
  , "SpinAttack"
  , "LeapAttack"
  , "WallAttack"
  , "ChannelingMultiplier"
  , "ChannelingCost"
  , "Ammo"
  ]

comparatorIListDamage :: [Text]
comparatorIListDamage =
  [ "Impact"
  , "Puncture"
  , "Slash"
  , "Heat"
  , "Cold"
  , "Toxin"
  , "Electricity"
  , "Blast"
  , "Gas"
  , "Radiation"
  , "Viral"
  , "Corrosive"
  , "Magnetic"
  ]

comparatorIListProcChances :: [Text]
comparatorIListProcChances =
  [ "Impact Proc Chance"
  , "Puncture Proc Chance"
  , "Slash Proc Chance"
  , "Heat Proc Chance"
  , "Cold Proc Chance"
  , "Toxin Proc Chance"
  , "Electricity Proc Chance"
  , "Blast Proc Chance"
  , "Gas Proc Chance"
  , "Radiation Proc Chance"
  , "Viral Proc Chance"
  , "Corrosive Proc Chance"
  , "Magnetic Proc Chance"
  ]

comparatorIListProcDPSs :: [Text]
comparatorIListProcDPSs =
  [ "Impact Proc DPS"
  , "Puncture Proc DPS"
  , "Slash Proc DPS"
  , "Heat Proc DPS"
  , "Cold Proc DPS"
  , "Toxin Proc DPS"
  , "Electricity Proc DPS"
  , "Blast Proc DPS"
  , "Gas Proc DPS"
  , "Radiation Proc DPS"
  , "Viral Proc DPS"
  , "Corrosive Proc DPS"
  , "Magnetic Proc DPS"
  ]

comparatorIListComprehensive :: [Text]
comparatorIListComprehensive =
  [ "TotalDamage"
  , "BurstDPS"
  , "SustainedDPS"
  , "AdjustedTotalDamage"
  , "AdjustedBurstDPS"
  , "AdjustedSustainedDPS"
  ]

comparatorIdentifier
  :: Text -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
comparatorIdentifier cmpr
  | bestMatch cmpr == Just "Ammo"
  = compareGenericWeaponGeneral gwAmmo
  | bestMatch cmpr == Just "MagazineSize"
  = compareGenericWeaponGeneral gwMagazineSize
  | bestMatch cmpr == Just "ReloadTime"
  = compareGenericWeaponGeneral gwReloadTime
  | bestMatch cmpr == Just "TotalDamage"
  = compareGenericWeaponGeneral gwTotalDamage
  | bestMatch cmpr == Just "BaseDamage"
  = compareGenericWeaponGeneral gwBaseDamage
  | bestMatch cmpr == Just "CriticalChance"
  = compareGenericWeaponGeneral gwCriticalChance
  | bestMatch cmpr == Just "CriticalMultiplier"
  = compareGenericWeaponGeneral gwCriticalMultiplier
  | bestMatch cmpr == Just "StatusChance"
  = compareGenericWeaponGeneral gwStatusChance
  | bestMatch cmpr == Just "FireRate"
  = compareGenericWeaponGeneral gwFireRate
  | bestMatch cmpr == Just "ChargeAttack"
  = compareGenericWeaponGeneral gwChargeAttack
  | bestMatch cmpr == Just "SpinAttack"
  = compareGenericWeaponGeneral gwSpinAttack
  | bestMatch cmpr == Just "LeapAttack"
  = compareGenericWeaponGeneral gwLeapAttack
  | bestMatch cmpr == Just "WallAttack"
  = compareGenericWeaponGeneral gwWallAttack
  | bestMatch cmpr == Just "ChannelingMultiplier"
  = compareGenericWeaponGeneral gwChannelingMultiplier
  | bestMatch cmpr == Just "ChannelingCost"
  = compareGenericWeaponGeneral gwChannelingCost
  | bestMatch cmpr == Just "Impact"
  = compareGenericWeaponDamage gdImpact
  | bestMatch cmpr == Just "Puncture"
  = compareGenericWeaponDamage gdPuncture
  | bestMatch cmpr == Just "Slash"
  = compareGenericWeaponDamage gdSlash
  | bestMatch cmpr == Just "Heat"
  = compareGenericWeaponDamage gdHeat
  | bestMatch cmpr == Just "Cold"
  = compareGenericWeaponDamage gdCold
  | bestMatch cmpr == Just "Toxin"
  = compareGenericWeaponDamage gdToxin
  | bestMatch cmpr == Just "Electricity"
  = compareGenericWeaponDamage gdElectricity
  | bestMatch cmpr == Just "Blast"
  = compareGenericWeaponDamage gdBlast
  | bestMatch cmpr == Just "Gas"
  = compareGenericWeaponDamage gdGas
  | bestMatch cmpr == Just "Radiation"
  = compareGenericWeaponDamage gdRadiation
  | bestMatch cmpr == Just "Viral"
  = compareGenericWeaponDamage gdViral
  | bestMatch cmpr == Just "Corrosive"
  = compareGenericWeaponDamage gdCorrosive
  | bestMatch cmpr == Just "Magnetic"
  = compareGenericWeaponDamage gdMagnetic
  | bestMatch cmpr == Just "TotalDamage"
  = compareTotalDamage
  | bestMatch cmpr == Just "BurstDPS"
  = compareBurstDPS
  | bestMatch cmpr == Just "SustainedDPS"
  = compareSustainedDPS
  | bestMatch cmpr == Just "AdjustedTotalDamage"
  = compareAdjustedTotalDamage
  | bestMatch cmpr == Just "AdjustedBurstDPS"
  = compareAdjustedBurstDPS
  | bestMatch cmpr == Just "AdjustedSustainedDPS"
  = compareAdjustedSustainedDPS
  | bestMatch cmpr == Just "Impact Proc Chance"
  = compareProcChances gpcImpact
  | bestMatch cmpr == Just "Puncture Proc Chance"
  = compareProcChances gpcPuncture
  | bestMatch cmpr == Just "Slash Proc Chance"
  = compareProcChances gpcSlash
  | bestMatch cmpr == Just "Heat Proc Chance"
  = compareProcChances gpcHeat
  | bestMatch cmpr == Just "Cold Proc Chance"
  = compareProcChances gpcCold
  | bestMatch cmpr == Just "Toxin Proc Chance"
  = compareProcChances gpcToxin
  | bestMatch cmpr == Just "Electricity Proc Chance"
  = compareProcChances gpcElectricity
  | bestMatch cmpr == Just "Blast Proc Chance"
  = compareProcChances gpcBlast
  | bestMatch cmpr == Just "Gas Proc Chance"
  = compareProcChances gpcGas
  | bestMatch cmpr == Just "Radiation Proc Chance"
  = compareProcChances gpcRadiation
  | bestMatch cmpr == Just "Viral Proc Chance"
  = compareProcChances gpcViral
  | bestMatch cmpr == Just "Corrosive Proc Chance"
  = compareProcChances gpcCorrosive
  | bestMatch cmpr == Just "Magnetic Proc Chance"
  = compareProcChances gpcMagnetic
  | bestMatch cmpr == Just "Impact Proc DPS"
  = compareProcDPSs gpdImpact
  | bestMatch cmpr == Just "Puncture Proc DPS"
  = compareProcDPSs gpdPuncture
  | bestMatch cmpr == Just "Slash Proc DPS"
  = compareProcDPSs gpdSlash
  | bestMatch cmpr == Just "Heat Proc DPS"
  = compareProcDPSs gpdHeat
  | bestMatch cmpr == Just "Cold Proc DPS"
  = compareProcDPSs gpdCold
  | bestMatch cmpr == Just "Toxin Proc DPS"
  = compareProcDPSs gpdToxin
  | bestMatch cmpr == Just "Electricity Proc DPS"
  = compareProcDPSs gpdElectricity
  | bestMatch cmpr == Just "Blast Proc DPS"
  = compareProcDPSs gpdBlast
  | bestMatch cmpr == Just "Gas Proc DPS"
  = compareProcDPSs gpdGas
  | bestMatch cmpr == Just "Radiation Proc DPS"
  = compareProcDPSs gpdRadiation
  | bestMatch cmpr == Just "Viral Proc DPS"
  = compareProcDPSs gpdViral
  | bestMatch cmpr == Just "Corrosive Proc DPS"
  = compareProcDPSs gpdCorrosive
  | bestMatch cmpr == Just "Magnetic Proc DPS"
  = compareProcDPSs gpdMagnetic
  | otherwise
  = compareAdjustedTotalDamage

-- | Converts a user comparator request into a Comparator type
comparatorGenerator
  :: Maybe Text -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
comparatorGenerator (Just txt) = comparatorIdentifier txt
comparatorGenerator Nothing    = compareAdjustedTotalDamage
