{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.MultishotMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify multishot, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.MultishotMods where

import           ClassyPrelude
import           Control.Lens
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

splitChamber3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
splitChamber3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 0.9)
  (+)
  targetWeapon
{-# INLINE splitChamber3 #-}

-- | Split Chamber [+90% Multishot]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
splitChamber
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
splitChamber baseWeapon (targetWeapon, mods) =
  ( splitChamber3 baseWeapon targetWeapon
  , "Split Chamber [+90% Multishot]" : mods
  )
{-# INLINE splitChamber #-}

vigilanteArmaments3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
vigilanteArmaments3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE vigilanteArmaments3 #-}

-- | Vigilante Armaments [+60% Multishot]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
vigilanteArmaments
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
vigilanteArmaments baseWeapon (targetWeapon, mods) =
  ( vigilanteArmaments3 baseWeapon targetWeapon
  , "Vigilante Armaments [+60% Multishot]" : mods
  )
{-# INLINE vigilanteArmaments #-}

