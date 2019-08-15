{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.MagazineMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify magazine size, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.MagazineMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

taintedMag1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
taintedMag1 baseWeapon targetWeapon = modifyGeneralProperty
  gwMagazineSize
  (baseWeapon ^. gwMagazineSize)
  (Just 0.66)
  (+)
  targetWeapon
{-# INLINE taintedMag1 #-}

taintedMag2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
taintedMag2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.33)
  (+)
  targetWeapon
{-# INLINE taintedMag2 #-}

-- | Tainted Mag [+66% Magazine, -33% Reload Speed (+33% Reload Time)]
taintedMag
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
taintedMag baseWeapon (targetWeapon, mods) =
  ( taintedMag1 baseWeapon $ taintedMag2 baseWeapon targetWeapon
  , "Tainted Mag [+66% Magazine, -33% Reload Speed (+33% Reload Time)]" : mods
  )
{-# INLINE taintedMag #-}
