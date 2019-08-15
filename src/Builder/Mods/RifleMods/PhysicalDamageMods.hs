{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.PhysicalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify physical damage, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.PhysicalDamageMods where

import           ClassyPrelude
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Piercing Caliber [+120% Puncture Damage]
piercingCaliber
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
piercingCaliber baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Piercing Caliber [+120% Puncture Damage]" : mods
  )
{-# INLINE piercingCaliber #-}

-- | Piercing Hit [+30% Puncture Damage]
piercingHit
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
piercingHit baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Piercing Hit [+30% Puncture Damage]" : mods
  )
{-# INLINE piercingHit #-}

-- | Fanged Fusillade [+120% Slash Damage]
fangedFusillade
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
fangedFusillade baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Fanged Fusillade [+120% Slash Damage]" : mods
  )
{-# INLINE fangedFusillade #-}

-- | Sawtooth Clip [+30% Slash Damage]
sawtoothClip
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sawtoothClip baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Sawtooth Clip [+30% Slash Damage]" : mods
  )
{-# INLINE sawtoothClip #-}

-- | Crash Course [+120% Impact Damage]
crashCourse
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
crashCourse baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Crash Course [+120% Impact Damage]" : mods
  )
{-# INLINE crashCourse #-}

-- | Rupture [+30% Impact Damage]
rupture :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
rupture baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Rupture [+30% Impact Damage]" : mods
  )
{-# INLINE rupture #-}
