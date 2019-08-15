{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify multishot, applicable on secondary weapons.

module Builder.Mods.PistolMods.MultishotMods where

import           ClassyPrelude
import           Control.Lens
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

barrelDiffusion3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
barrelDiffusion3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 1.2)
  (+)
  targetWeapon
{-# INLINE barrelDiffusion3 #-}

-- | Barrel Diffusion [+120% Multishot]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
barrelDiffusion
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
barrelDiffusion baseWeapon (targetWeapon, mods) =
  ( barrelDiffusion3 baseWeapon targetWeapon
  , "Barrel Diffusion [+120% Multishot]" : mods
  )
{-# INLINE barrelDiffusion #-}

lethalTorrent5 :: GenericWeapon -> GenericWeapon -> GenericWeapon
lethalTorrent5 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE lethalTorrent5 #-}

lethalTorrent3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
lethalTorrent3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE lethalTorrent3 #-}

-- | Lethal Torrent [+60% Multishot, +60% Fire Rate]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
lethalTorrent
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
lethalTorrent baseWeapon (targetWeapon, mods) =
  ( lethalTorrent3 baseWeapon $ lethalTorrent5 baseWeapon targetWeapon
  , "Lethal Torrent [+60% Multishot]" : mods
  )
{-# INLINE lethalTorrent #-}

amalgamBarrelDiffusion3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
amalgamBarrelDiffusion3 baseWeapon targetWeapon = modifyGeneralProperty
  gwMultishot
  (baseWeapon ^. gwMultishot)
  (Just 1.1)
  (+)
  targetWeapon
{-# INLINE amalgamBarrelDiffusion3 #-}

-- | Amalgam Barrel Diffusion [+110% Multishot]
--   Status Chance affected by multishot
--   1 - ((1 - status chance) ^ multishot modifier)
amalgamBarrelDiffusion
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
amalgamBarrelDiffusion baseWeapon (targetWeapon, mods) =
  ( amalgamBarrelDiffusion3 baseWeapon targetWeapon
  , "Amalgam Barrel Diffusion [+120% Multishot]" : mods
  )
{-# INLINE amalgamBarrelDiffusion #-}
