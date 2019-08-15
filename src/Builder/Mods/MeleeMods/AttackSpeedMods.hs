{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.MeleeMods.AttackSpeedMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify attack speed, applicable on melee weapons.

module Builder.Mods.MeleeMods.AttackSpeedMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

quickening1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
quickening1 baseWeapon targetWeapon = modifyGeneralProperty
  gwChannelingCost
  (baseWeapon ^. gwChannelingCost)
  (Just 0.5)
  (+)
  targetWeapon
{-# INLINE quickening1 #-}

quickening2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
quickening2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.2)
  (+)
  targetWeapon
{-# INLINE quickening2 #-}

-- | Quickening [+20% Attack Speed, -50% Channeling Efficiency]
quickening
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
quickening baseWeapon (targetWeapon, mods) =
  ( quickening1 baseWeapon $ quickening2 baseWeapon targetWeapon
  , "Quickening [+20% Attack Speed, -50% Channeling Efficiency]" : mods
  )
{-# INLINE quickening #-}

-- | Berserker [+75% Attack Speed]
berserker :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
berserker baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.75)
                          (+)
                          targetWeapon
  , "Berserker [+75% Attack Speed]" : mods
  )
{-# INLINE berserker #-}

-- | Fury [+30% Attack Speed]
fury :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
fury baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.30)
                          (+)
                          targetWeapon
  , "Fury [+30% Attack Speed]" : mods
  )
{-# INLINE fury #-}

-- | Primed Fury [+55% Attack Speed]
primedFury
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedFury baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.55)
                          (+)
                          targetWeapon
  , "Primed Fury [+55% Attack Speed]" : mods
  )
{-# INLINE primedFury #-}

-- | Gladiator Vice [+30% Attack Speed]
gladiatorVice
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
gladiatorVice baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.30)
                          (+)
                          targetWeapon
  , "Gladiator Vice [+30% Attack Speed]" : mods
  )
{-# INLINE gladiatorVice #-}
