{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.CriticalMultiplierMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical multiplier, applicable on primary (rifle) weapons.


module Builder.Mods.RifleMods.CriticalMultiplierMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Vital Sense [+120% Critical Multiplier]
vitalSense
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
vitalSense baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 1.2)
                          (+)
                          targetWeapon
  , "Vital Sense [+120% Critical Multiplier]" : mods
  )
{-# INLINE vitalSense #-}

-- | Bladed Rounds [+120% Critical Multiplier, on kill]
bladedRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
bladedRounds baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 1.2)
                          (+)
                          targetWeapon
  , "Bladed Rounds [+120% Critical Multiplier, on kill]" : mods
  )
{-# INLINE bladedRounds #-}

hammerShot1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hammerShot1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalMultiplier
  (baseWeapon ^. gwCriticalMultiplier)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE hammerShot1 #-}

hammerShot2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hammerShot2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.4)
  (+)
  targetWeapon
{-# INLINE hammerShot2 #-}

-- | Hammer Shot [+60% Critical Multiplier, +40% Status Chance]
hammerShot
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hammerShot baseWeapon (targetWeapon, mods) =
  ( hammerShot2 baseWeapon $ hammerShot1 baseWeapon targetWeapon
  , "Hammer Shot [+60% Critical Multiplier, +40% Status Chance]" : mods
  )
{-# INLINE hammerShot #-}
