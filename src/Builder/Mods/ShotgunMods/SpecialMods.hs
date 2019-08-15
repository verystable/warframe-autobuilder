{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.SpecialMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify special features, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.SpecialMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]
hunterMunitions
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hunterMunitions _ (targetWeapon, mods) =
  ( targetWeapon
  , "Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]"
    : mods
  )
{-# INLINE hunterMunitions #-}

-- | Hunter Track [+30% Status Duration]
hunterTrack
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hunterTrack _ (targetWeapon, mods) =
  (targetWeapon, "Hunter Track [+30% Status Duration]" : mods)
{-# INLINE hunterTrack #-}

-- | Lingering Torment [+30% Status Duration]
lingeringTorment
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
lingeringTorment _ (targetWeapon, mods) =
  (targetWeapon, "Lingering Torment [+30% Status Duration]" : mods)
{-# INLINE lingeringTorment #-}

-- | Seeking Force [+2.1m Punchthrough]
seekingForce
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
seekingForce _ (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 2.1) (Just 1) (+) targetWeapon
  , "Seeking Force [+2.1m Punchthrough]" : mods
  )
{-# INLINE seekingForce #-}

-- | Seeking Fury [-15% Reload Time, +1.2m Punchthrough]
seekingFury
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
seekingFury baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 1.2) (Just 1) (+)
    $ modifyGeneralProperty gwReloadTime
                            (baseWeapon ^. gwReloadTime)
                            (Just 0.15)
                            (-)
                            targetWeapon
  , "Seeking Fury [-15% Reload Time, +1.2m Punchthrough]" : mods
  )
{-# INLINE seekingFury #-}
