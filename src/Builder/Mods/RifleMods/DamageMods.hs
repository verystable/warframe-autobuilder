{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.DamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify damage, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.DamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

serration1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
serration1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.65)
  (+)
  targetWeapon
{-# INLINE serration1 #-}

serration2 :: GenericWeapon -> GenericWeapon
serration2 = applyMultiplier (Just 2.65)
{-# INLINE serration2 #-}

-- | Serration [+165% Damage]
serration :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
serration baseWeapon (targetWeapon, mods) =
  ( serration1 baseWeapon $ serration2 targetWeapon
  , "Serration [+165% Damage]" : mods
  )
{-# INLINE serration #-}

amalgamSerration1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
amalgamSerration1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 2.55)
  (+)
  targetWeapon
{-# INLINE amalgamSerration1 #-}

amalgamSerration2 :: GenericWeapon -> GenericWeapon
amalgamSerration2 = applyMultiplier (Just 2.55)
{-# INLINE amalgamSerration2 #-}

-- | Amalgam Serration [+155% Damage, +25% Sprint Speed]
amalgamSerration
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
amalgamSerration baseWeapon (targetWeapon, mods) =
  ( amalgamSerration1 baseWeapon $ amalgamSerration2 targetWeapon
  , "Amalgam Serration [+155% Damage, +25% Sprint Speed]" : mods
  )
{-# INLINE amalgamSerration #-}
