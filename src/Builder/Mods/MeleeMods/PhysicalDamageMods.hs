{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.PhysicalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify physical damage, applicable on melee weapons.

module Builder.Mods.MeleeMods.PhysicalDamageMods where

import           ClassyPrelude
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Collision Force [+120% Impact Damage]
collisionForce
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
collisionForce baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Collision Force [+120% Impact Damage]" : mods
  )
{-# INLINE collisionForce #-}

-- | Heavy Trauma [+90% Impact Damage]
heavyTrauma
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
heavyTrauma baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Heavy Trauma [+90% Impact Damage]" : mods
  )
{-# INLINE heavyTrauma #-}

-- | Primed Heavy Trauma [+165% Impact Damage]
primedHeavyTrauma
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedHeavyTrauma baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdImpact
                         (getGenericDamageProperty baseWeapon gdImpact)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Heavy Trauma [+165% Impact Damage]" : mods
  )
{-# INLINE primedHeavyTrauma #-}

-- | Sundering Strike [+90% Puncture Damage]
sunderingStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sunderingStrike baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Sundering Strike [+90% Puncture Damage]" : mods
  )
{-# INLINE sunderingStrike #-}

-- | Auger Strike [+120% Puncture Damage]
augerStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
augerStrike baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Auger Strike [+120% Puncture Damage]" : mods
  )
{-# INLINE augerStrike #-}

-- | Primed Auger Strike [+165% Puncture Damage]
primedAugerStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedAugerStrike baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdPuncture
                         (getGenericDamageProperty baseWeapon gdPuncture)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Auger Strike [+165% Puncture Damage]" : mods
  )
{-# INLINE primedAugerStrike #-}

-- | Buzz Kill [+120% Slash Damage]
buzzKill :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
buzzKill baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 1.2)
                         (+)
                         targetWeapon
  , "Buzz Kill [+120% Slash Damage]" : mods
  )
{-# INLINE buzzKill #-}

-- | Primed Buzz Kill [+165% Slash Damage]
primedBuzzKill
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedBuzzKill baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Buzz Kill [+165% Slash Damage]" : mods
  )
{-# INLINE primedBuzzKill #-}

-- | jaggedEdge [+90% Slash Damage]
jaggedEdge
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
jaggedEdge baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdSlash
                         (getGenericDamageProperty baseWeapon gdSlash)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "JaggedEdge [+90% Slash Damage]" : mods
  )
{-# INLINE jaggedEdge #-}

rendingStrike1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
rendingStrike1 baseWeapon targetWeapon = modifyDamageProperty
  gdSlash
  (getGenericDamageProperty baseWeapon gdSlash)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE rendingStrike1 #-}

rendingStrike2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
rendingStrike2 baseWeapon targetWeapon = modifyDamageProperty
  gdPuncture
  (getGenericDamageProperty baseWeapon gdPuncture)
  (Just 0.8)
  (+)
  targetWeapon
{-# INLINE rendingStrike2 #-}

-- | Rending Strike [+60% Slash Damage, +80% Puncture Damage]
rendingStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
rendingStrike baseWeapon (targetWeapon, mods) =
  ( rendingStrike1 baseWeapon $ rendingStrike2 baseWeapon targetWeapon
  , "Rending Strike [+60% Slash Damage, +80% Puncture Damage]" : mods
  )
{-# INLINE rendingStrike #-}
