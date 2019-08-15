{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.DamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify damage, applicable on secondary weapons.

module Builder.Mods.PistolMods.DamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

hornetStrike1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
hornetStrike1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 2.2)
  (+)
  targetWeapon
{-# INLINE hornetStrike1 #-}

hornetStrike2 :: GenericWeapon -> GenericWeapon
hornetStrike2 = applyMultiplier (Just 3.2)
{-# INLINE hornetStrike2 #-}

-- | Hornet Strike [+165% Damage]
hornetStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hornetStrike baseWeapon (targetWeapon, mods) =
  ( hornetStrike1 baseWeapon $ hornetStrike2 targetWeapon
  , "Hornet Strike [+165% Damage]" : mods
  )
{-# INLINE hornetStrike #-}

augurPact1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
augurPact1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE augurPact1 #-}

augurPact2 :: GenericWeapon -> GenericWeapon
augurPact2 = applyMultiplier (Just 1.6)
{-# INLINE augurPact2 #-}

-- | Augur Pact [+60% Damage]
augurPact :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
augurPact baseWeapon (targetWeapon, mods) =
  ( augurPact1 baseWeapon $ augurPact2 targetWeapon
  , "Augur Pact [+60% Damage]" : mods
  )
{-# INLINE augurPact #-}
