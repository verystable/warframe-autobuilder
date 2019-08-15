{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.MeleeMods.CriticalMultiplierMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical multiplier, applicable on secondary weapons.

module Builder.Mods.PistolMods.CriticalMultiplierMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Primed Target Cracker [+110% Critical Multiplier]
primedTargetCracker
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedTargetCracker baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 1.1)
                          (+)
                          targetWeapon
  , "Primed Target Cracker [+110% Critical Multiplier]" : mods
  )
{-# INLINE primedTargetCracker #-}

-- | Target Cracker [+60% Critical Multiplier]
targetCracker
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
targetCracker baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Target Cracker [+60% Critical Multiplier]" : mods
  )
{-# INLINE targetCracker #-}

-- | Sharpened Bullets [+75% Critical Multiplier, on kill]
sharpenedBullets
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sharpenedBullets baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.75)
                          (+)
                          targetWeapon
  , "Sharpened Bullets [+75% Critical Multiplier, on kill]" : mods
  )
{-# INLINE sharpenedBullets #-}

hollowPoint1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hollowPoint1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalMultiplier
  (baseWeapon ^. gwCriticalMultiplier)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE hollowPoint1 #-}

hollowPoint2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hollowPoint2 baseWeapon targetWeapon =
  modifyWithMultiplier (Just 0.15) (-) targetWeapon
{-# INLINE hollowPoint2 #-}

-- | Creeping Bullseye [+60% Critical Multiplier, -15% Damage]
hollowPoint
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hollowPoint baseWeapon (targetWeapon, mods) =
  ( hollowPoint1 baseWeapon $ hollowPoint2 baseWeapon targetWeapon
  , "Creeping Bullseye [+60% Critical Multiplier, -15% Damage]" : mods
  )
{-# INLINE hollowPoint #-}
