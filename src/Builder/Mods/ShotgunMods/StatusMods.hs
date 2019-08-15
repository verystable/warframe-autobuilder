{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.StatusMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify status, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.StatusMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Nano-Applicator [+90% Status Chance, on ability cast]
nanoApplicator
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
nanoApplicator baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Nano-Applicator [+90% Status Chance, on ability cast]" : mods
  )
{-# INLINE nanoApplicator #-}

-- | Shotgun Savvy [+30% Status Chance]
shotgunSavvy
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shotgunSavvy baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.3)
                          (+)
                          targetWeapon
  , "Shotgun Savvy [+30% Status Chance]" : mods
  )
{-# INLINE shotgunSavvy #-}
