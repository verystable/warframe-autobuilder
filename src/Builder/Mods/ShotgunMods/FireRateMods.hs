{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.FireRateMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify fire rate, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.FireRateMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

frailMomentum1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frailMomentum1 _ targetWeapon =
  modifyWithMultiplier (Just 0.15) (-) targetWeapon
{-# INLINE frailMomentum1 #-}

frailMomentum2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frailMomentum2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.9)
  (+)
  targetWeapon
{-# INLINE frailMomentum2 #-}

-- | Frail Momentum [+90% Fire Rate, -15% Damage]
frailMomentum
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
frailMomentum baseWeapon (targetWeapon, mods) =
  ( frailMomentum1 baseWeapon $ frailMomentum2 baseWeapon targetWeapon
  , "Frail Momentum [+90% Fire Rate, -15% Damage]" : mods
  )
{-# INLINE frailMomentum #-}

-- | Repeater Clip [+105% Fire Rate, on reload]
repeaterClip
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
repeaterClip baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 1.05)
                          (+)
                          targetWeapon
  , "Repeater Clip [+105% Fire Rate, on reload]" : mods
  )
{-# INLINE repeaterClip #-}

-- | Shotgun Spazz [+90% Fire Rate]
shotgunSpazz
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shotgunSpazz baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Shotgun Spazz [+90% Fire Rate]" : mods
  )
{-# INLINE shotgunSpazz #-}
