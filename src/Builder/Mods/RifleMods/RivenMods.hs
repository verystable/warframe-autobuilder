{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.RivenMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains Riven mods or functions.

module Builder.Mods.RifleMods.RivenMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Rifle Example Riven | Soma [+120% Critical Chance, -50% Reload Time]
rifleExampleRiven
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
rifleExampleRiven baseWeapon (targetWeapon, mods)
  | targetWeapon ^. gwName == Just "Soma" = applyModification
  | targetWeapon ^. gwName == Just "Soma Prime" = applyModification
  | otherwise                             = (targetWeapon, mods)
 where
  applyModification =
    ( rifleExampleRivenModification baseWeapon targetWeapon
    , "Rifle Example Riven | Soma [+120% Critical Chance, -50% Reload Time]"
      : mods
    )

rifleExampleRivenModification1
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
rifleExampleRivenModification1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 1.2)
  (+)
  targetWeapon

rifleExampleRivenModification2
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
rifleExampleRivenModification2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.5)
  (-)
  targetWeapon

rifleExampleRivenModification :: GenericWeapon -> GenericWeapon -> GenericWeapon
rifleExampleRivenModification baseWeapon targetWeapon =
  rifleExampleRivenModification1 baseWeapon
    $ rifleExampleRivenModification2 baseWeapon targetWeapon


