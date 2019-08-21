{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.RivenMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains Riven mods or functions.

module Builder.Mods.PistolMods.RivenMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Pistol Example Riven | Lex [+120% Critical Chance, -50% Reload Time]
pistolExampleRiven
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pistolExampleRiven baseWeapon (targetWeapon, mods)
  | targetWeapon ^. gwName == Just "Lex" = applyModification
  | targetWeapon ^. gwName == Just "Lex Prime" = applyModification
  | otherwise                            = (targetWeapon, mods)
 where
  applyModification =
    ( pistolExampleRivenModification baseWeapon targetWeapon
    , "Pistol Example Riven | Lex [+120% Critical Chance, -50% Reload Time]"
      : mods
    )

pistolExampleRivenModification1
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
pistolExampleRivenModification1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 1.2)
  (+)
  targetWeapon

pistolExampleRivenModification2
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
pistolExampleRivenModification2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.5)
  (-)
  targetWeapon

pistolExampleRivenModification
  :: GenericWeapon -> GenericWeapon -> GenericWeapon
pistolExampleRivenModification baseWeapon targetWeapon =
  pistolExampleRivenModification1 baseWeapon
    $ pistolExampleRivenModification2 baseWeapon targetWeapon


