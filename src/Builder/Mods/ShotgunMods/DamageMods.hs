{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.DamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify damage, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.DamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

pointBlank1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
pointBlank1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 0.9)
  (+)
  targetWeapon
{-# INLINE pointBlank1 #-}

pointBlank2 :: GenericWeapon -> GenericWeapon
pointBlank2 = applyMultiplier (Just 1.9)
{-# INLINE pointBlank2 #-}

-- | Point Blank [+90% Damage]
pointBlank
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pointBlank baseWeapon (targetWeapon, mods) =
  ( pointBlank1 baseWeapon $ pointBlank2 targetWeapon
  , "Point Blank [+165% Damage]" : mods
  )
{-# INLINE pointBlank #-}

primedPointBlank1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
primedPointBlank1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.65)
  (+)
  targetWeapon
{-# INLINE primedPointBlank1 #-}

primedPointBlank2 :: GenericWeapon -> GenericWeapon
primedPointBlank2 = applyMultiplier (Just 2.65)
{-# INLINE primedPointBlank2 #-}

-- | Primed Point Blank [+165% Damage]
primedPointBlank
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedPointBlank baseWeapon (targetWeapon, mods) =
  ( primedPointBlank1 baseWeapon $ primedPointBlank2 targetWeapon
  , "Primed Point Blank [+165% Damage]" : mods
  )
{-# INLINE primedPointBlank #-}
