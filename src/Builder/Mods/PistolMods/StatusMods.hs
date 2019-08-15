{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify status, applicable on secondary weapons.

module Builder.Mods.PistolMods.StatusMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Embedded Catalyzer [+90% Status Chance, on ability cast]
embeddedCatalyzer
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
embeddedCatalyzer baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Embedded Catalyzer [+90% Status Chance, on ability cast]" : mods
  )
{-# INLINE embeddedCatalyzer #-}

-- | Sure Shot [+15% Status Chance]
sureShot :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sureShot baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.15)
                          (+)
                          targetWeapon
  , "Sure Shot [+15% Status Chance]" : mods
  )
{-# INLINE sureShot #-}

stunningSpeed1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
stunningSpeed1 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.4)
  (+)
  targetWeapon
{-# INLINE stunningSpeed1 #-}

stunningSpeed2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
stunningSpeed2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.1)
  (+)
  targetWeapon
{-# INLINE stunningSpeed2 #-}

-- | Stunning Speed [+40% Reload Speed, +10% Status Chance]
stunningSpeed
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
stunningSpeed baseWeapon (targetWeapon, mods) =
  ( stunningSpeed1 baseWeapon $ stunningSpeed2 baseWeapon targetWeapon
  , "Stunning Speed [+40% Reload Speed, +10% Status Chance]" : mods
  )
{-# INLINE stunningSpeed #-}
