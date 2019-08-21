{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.MeleeMods.RivenMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains Riven mods or functions.

module Builder.Mods.MeleeMods.RivenMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Melee Example Riven | Skana [+120% Critical Chance, +50% Damage]
meleeExampleRiven
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
meleeExampleRiven baseWeapon (targetWeapon, mods)
  | targetWeapon ^. gwName == Just "Skana" = applyModification
  | targetWeapon ^. gwName == Just "Skana Prime" = applyModification
  | otherwise                              = (targetWeapon, mods)
 where
  applyModification =
    ( meleeExampleRivenModification baseWeapon targetWeapon
    , "Melee Example Riven | Skana [+120% Critical Chance, -50% Reload Time]"
      : mods
    )

meleeExampleRivenModification1
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
meleeExampleRivenModification1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 1.2)
  (+)
  targetWeapon

meleeExampleRivenModification2
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
meleeExampleRivenModification2 baseWeapon targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (baseWeapon ^. gwBaseDamage)
  (Just 0.5)
  (+)
  targetWeapon

meleeExampleRivenModification :: GenericWeapon -> GenericWeapon -> GenericWeapon
meleeExampleRivenModification baseWeapon targetWeapon =
  meleeExampleRivenModification1 baseWeapon
    $ meleeExampleRivenModification2 baseWeapon targetWeapon


