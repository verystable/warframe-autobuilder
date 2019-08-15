{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.MultishotMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify multishot, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.MultishotMods where

import           ClassyPrelude
import           Control.Lens
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

hellsChamber3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hellsChamber3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 1.2)
  (+)
  targetWeapon
{-# INLINE hellsChamber3 #-}

-- | Hell's Chamber [+120% Multishot]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
hellsChamber
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hellsChamber baseWeapon (targetWeapon, mods) =
  ( hellsChamber3 baseWeapon targetWeapon
  , "Hell's Chamber [+120% Multishot]" : mods
  )
{-# INLINE hellsChamber #-}
