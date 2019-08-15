{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify physical damage, applicable on secondary weapons.

module Builder.Mods.PistolMods.PhysicalDamageMods where

import           ClassyPrelude
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Bore [+120% Puncture Damage]
bore :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
bore baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Bore [+120% Puncture Damage]" : mods
  )
{-# INLINE bore #-}

-- | No Return [+30% Puncture Damage]
noReturn :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
noReturn baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "No Return [+30% Puncture Damage]" : mods
  )
{-# INLINE noReturn #-}

-- | Maim [+120% Slash Damage]
maim :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
maim baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Maim [+120% Slash Damage]" : mods
  )
{-# INLINE maim #-}

-- | Razor Shot [+30% Slash Damage]
razorShot :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
razorShot baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Razor Shot [+30% Slash Damage]" : mods
  )
{-# INLINE razorShot #-}

-- | Pummel [+120% Impact Damage]
pummel :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pummel baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Pummel [+120% Impact Damage]" : mods
  )
{-# INLINE pummel #-}

-- | Concussion Rounds [+30% Impact Damage]
concussionRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
concussionRounds baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Concussion Rounds [+30% Impact Damage]" : mods
  )
{-# INLINE concussionRounds #-}
