{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.CriticalChanceMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical chance, applicable on secondary weapons.

module Builder.Mods.PistolMods.CriticalChanceMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Pistol Gambit [+120% Critical Chance]
pistolGambit
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pistolGambit baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.2)
                          (+)
                          targetWeapon
  , "Pistol Gambit [+120% Critical Chance]" : mods
  )
{-# INLINE pistolGambit #-}

-- | Primed Pistol Gambit [+187% Critical Chance]
primedPistolGambit
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedPistolGambit baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.87)
                          (+)
                          targetWeapon
  , "Primed Pistol Gambit [+187% Critical Chance]" : mods
  )
{-# INLINE primedPistolGambit #-}

-- | Hydraulic Crosshairs [+135% Critical Chance, on headshot]
hydraulicCrosshairs
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hydraulicCrosshairs baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.35)
                          (+)
                          targetWeapon
  , "Hydraulic Crosshairs [+135% Critical Chance, on headshot]" : mods
  )
{-# INLINE hydraulicCrosshairs #-}

creepingBullseye1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
creepingBullseye1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 0.48)
  (+)
  targetWeapon
{-# INLINE creepingBullseye1 #-}

creepingBullseye2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
creepingBullseye2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.36)
  (-)
  targetWeapon
{-# INLINE creepingBullseye2 #-}

-- | Creeping Bullseye [+48% Critical Chance, -36% Fire Rate]
creepingBullseye
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
creepingBullseye baseWeapon (targetWeapon, mods) =
  ( creepingBullseye1 baseWeapon $ creepingBullseye2 baseWeapon targetWeapon
  , "Creeping Bullseye [+48% Critical Chance, -36% Fire Rate]" : mods
  )
{-# INLINE creepingBullseye #-}
