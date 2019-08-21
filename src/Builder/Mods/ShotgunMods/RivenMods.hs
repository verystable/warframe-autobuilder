{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.RivenMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains Riven mods or functions.

module Builder.Mods.ShotgunMods.RivenMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Shotgun Example Riven | Boar [+120% Critical Chance, -50% Reload Time]
shotgunExampleRiven
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shotgunExampleRiven baseWeapon (targetWeapon, mods)
  | targetWeapon ^. gwName == Just "Boar" = applyModification
  | targetWeapon ^. gwName == Just "Boar Prime" = applyModification
  | otherwise                             = (targetWeapon, mods)
 where
  applyModification =
    ( shotgunExampleRivenModification baseWeapon targetWeapon
    , "Shotgun Example Riven | Boar [+120% Critical Chance, -50% Reload Time]"
      : mods
    )

shotgunExampleRivenModification1
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
shotgunExampleRivenModification1 baseWeapon targetWeapon =
  modifyGeneralProperty gwCriticalChance
                        (baseWeapon ^. gwCriticalChance)
                        (Just 1.2)
                        (+)
                        targetWeapon

shotgunExampleRivenModification2
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
shotgunExampleRivenModification2 baseWeapon targetWeapon =
  modifyGeneralProperty gwReloadTime
                        (baseWeapon ^. gwReloadTime)
                        (Just 0.5)
                        (-)
                        targetWeapon

shotgunExampleRivenModification
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
shotgunExampleRivenModification baseWeapon targetWeapon =
  shotgunExampleRivenModification1 baseWeapon
    $ shotgunExampleRivenModification2 baseWeapon targetWeapon
