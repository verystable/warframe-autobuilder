{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.CriticalChanceMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify criticalChance, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.CriticalChanceMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Blunderbuss [+90% Critical Chance]
blunderbuss
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
blunderbuss baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Blunderbuss [+90% Critical Chance]" : mods
  )
{-# INLINE blunderbuss #-}

-- | Laser Sight [+120% Critical Chance, on headshot]
laserSight
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
laserSight baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.2)
                          (+)
                          targetWeapon
  , "Laser Sight [+120% Critical Chance, on headshot]" : mods
  )
{-# INLINE laserSight #-}

criticalDeceleration1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
criticalDeceleration1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 0.48)
  (+)
  targetWeapon
{-# INLINE criticalDeceleration1 #-}

criticalDeceleration2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
criticalDeceleration2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.3)
  (+)
  targetWeapon
{-# INLINE criticalDeceleration2 #-}

-- | Critical Deceleration [+48% Critical Chance, -30% Fire Rate]
criticalDeceleration
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
criticalDeceleration baseWeapon (targetWeapon, mods) =
  ( criticalDeceleration1 baseWeapon
    $ criticalDeceleration2 baseWeapon targetWeapon
  , "Critical Deceleration [+48% Critical Chance, -30% Fire Rate]" : mods
  )
{-# INLINE criticalDeceleration #-}
