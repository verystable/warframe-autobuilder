{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.CriticalChanceMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify criticalChance, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.CriticalMultiplierMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Primed Ravage [+110% Critical Multiplier]
primedRavage
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedRavage baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 1.1)
                          (+)
                          targetWeapon
  , "Primed Ravage [+110% Critical Multiplier]" : mods
  )
{-# INLINE primedRavage #-}

-- | Ravage [+60% Critical Multiplier]
ravage :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
ravage baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Ravage [+60% Critical Multiplier]" : mods
  )
{-# INLINE ravage #-}

-- | Sharpnel Shot [+99% Critical Multiplier, on kill]
sharpnelShot
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sharpnelShot baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.99)
                          (+)
                          targetWeapon
  , "Sharpnel Shot [+99% Critical Multiplier, on kill]" : mods
  )
{-# INLINE sharpnelShot #-}
