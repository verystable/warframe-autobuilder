{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.PhysicalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify physical damage, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.PhysicalDamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

acceleratedBlast1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
acceleratedBlast1 baseWeapon targetWeapon = modifyDamageProperty
  gdPuncture
  (getGenericDamageProperty baseWeapon gdPuncture)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE acceleratedBlast1 #-}

acceleratedBlast2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
acceleratedBlast2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE acceleratedBlast2 #-}

-- | Accelerated Blast [+60% Fire Rate, +60% Puncture Damage]
acceleratedBlast
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
acceleratedBlast baseWeapon (targetWeapon, mods) =
  ( acceleratedBlast1 baseWeapon $ acceleratedBlast2 baseWeapon targetWeapon
  , "Accelerated Blast [+60% Fire Rate, +60% Puncture Damage]" : mods
  )
{-# INLINE acceleratedBlast #-}

-- | Breach Loader [+120% Puncture Damage]
breachLoader
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
breachLoader baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Breach Loader [+120% Puncture Damage]" : mods
  )
{-# INLINE breachLoader #-}

-- | Flechette [+30% Puncture Damage]
flechette :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
flechette baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Flechette [+30% Puncture Damage]" : mods
  )
{-# INLINE flechette #-}

-- | Sweeping Serration [+120% Slash Damage]
sweepingSerration
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sweepingSerration baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Sweeping Serration [+120% Slash Damage]" : mods
  )
{-# INLINE sweepingSerration #-}

-- | shredder [+30% Slash Damage]
shredder :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shredder baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Shredder [+30% Slash Damage]" : mods
  )
{-# INLINE shredder #-}

-- | Full Contact [+120% Impact Damage]
fullContact
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
fullContact baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Full Contact [+120% Impact Damage]" : mods
  )
{-# INLINE fullContact #-}

-- | Disruptor [+30% Impact Damage]
disruptor :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
disruptor baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 0.3)
                         (+)
                         targetWeapon
  , "Disruptor [+30% Impact Damage]" : mods
  )
{-# INLINE disruptor #-}
