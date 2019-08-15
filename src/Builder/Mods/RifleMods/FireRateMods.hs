{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.FireRateMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify fire rate, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.FireRateMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

vileAcceleration1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
vileAcceleration1 _ targetWeapon =
  modifyWithMultiplier (Just 0.15) (-) targetWeapon
{-# INLINE vileAcceleration1 #-}

vileAcceleration2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
vileAcceleration2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.9)
  (+)
  targetWeapon
{-# INLINE vileAcceleration2 #-}

-- | Vile Acceleration [+90% Fire Rate, -15% Damage]
vileAcceleration
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
vileAcceleration baseWeapon (targetWeapon, mods) =
  ( vileAcceleration1 baseWeapon $ vileAcceleration2 baseWeapon targetWeapon
  , "Vile Acceleration [+90% Fire Rate, -15% Damage]" : mods
  )
{-# INLINE vileAcceleration #-}

-- | Spring-Loaded Chamber [+75% Fire Rate, on reload]
springLoadedChamber
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
springLoadedChamber baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.75)
                          (+)
                          targetWeapon
  , "Spring-Loaded Chamber [+75% Fire Rate, on reload]" : mods
  )
{-# INLINE springLoadedChamber #-}

-- | Speed Trigger [+60% Fire Rate]
speedTrigger
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
speedTrigger baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Speed Trigger [+60% Fire Rate]" : mods
  )
{-# INLINE speedTrigger #-}
