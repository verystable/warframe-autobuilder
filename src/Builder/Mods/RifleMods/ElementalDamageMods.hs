{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.ElementalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify elemental damage, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.ElementalDamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Elemental Mods

-- | Cryo Rounds [+90% Cold Damage]
cryoRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
cryoRounds baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Cryo Rounds [+90% Cold Damage]" : mods
  )
{-# INLINE cryoRounds #-}

-- | Primed Cryo Rounds [+165% Cold Damage]
primedCryoRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedCryoRounds baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Cryo Rounds [+165% Cold Damage]" : mods
  )
{-# INLINE primedCryoRounds #-}

-- | Hellfire [+90% Heat Damage]
hellfire :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hellfire baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Hellfire [+90% Heat Damage]" : mods
  )
{-# INLINE hellfire #-}

-- | Primed Hellfire [+165% Heat Damage]
primedHellfire
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedHellfire baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Hellfire [+165% Heat Damage]" : mods
  )
{-# INLINE primedHellfire #-}

-- | Infected Clip [+90% Toxin Damage]
infectedClip
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
infectedClip baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Infected Clip [+90% Toxin Damage]" : mods
  )
{-# INLINE infectedClip #-}

-- | Primed Infected Clip [+165% Toxin Damage]
primedInfectedClip
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedInfectedClip baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Infected Clip [+165% Toxin Damage]" : mods
  )
{-# INLINE primedInfectedClip #-}

-- | Stormbringer [+90% Electricity Damage]
stormbringer
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
stormbringer baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Stormbringer [+90% Electricity Damage]" : mods
  )
{-# INLINE stormbringer #-}

-- | Primed Stormbringer [+165% Electricity Damage]
primedStormbringer
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedStormbringer baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Stormbringer [+165% Electricity Damage]" : mods
  )
{-# INLINE primedStormbringer #-}

-- | Dual Stat Status Mods

rimeRounds1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
rimeRounds1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE rimeRounds1 #-}

rimeRounds2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
rimeRounds2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE rimeRounds2 #-}

-- | Rime Rounds [+60% Cold Damage, +60% Status Chance]
rimeRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
rimeRounds baseWeapon (targetWeapon, mods) =
  ( rimeRounds1 baseWeapon $ rimeRounds2 baseWeapon targetWeapon
  , "Rime Rounds [+60% Cold Damage, +60% Status Chance]" : mods
  )
{-# INLINE rimeRounds #-}

thermiteRounds1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
thermiteRounds1 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE thermiteRounds1 #-}

thermiteRounds2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
thermiteRounds2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE thermiteRounds2 #-}

-- | Thermite Rounds [+60% Heat Damage, +60% Status Chance]
thermiteRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
thermiteRounds baseWeapon (targetWeapon, mods) =
  ( thermiteRounds1 baseWeapon $ thermiteRounds2 baseWeapon targetWeapon
  , "Thermite Rounds [+60% Heat Damage, +60% Status Chance]" : mods
  )
{-# INLINE thermiteRounds #-}

malignantForce1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
malignantForce1 baseWeapon targetWeapon = modifyDamageProperty
  gdToxin
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE malignantForce1 #-}

malignantForce2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
malignantForce2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE malignantForce2 #-}

-- | Malignant Force [+60% Toxin Damage, +60% Status Chance]
malignantForce
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
malignantForce baseWeapon (targetWeapon, mods) =
  ( malignantForce1 baseWeapon $ malignantForce2 baseWeapon targetWeapon
  , "Malignant Force [+60% Toxin Damage, +60% Status Chance]" : mods
  )
{-# INLINE malignantForce #-}

highVoltage1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
highVoltage1 baseWeapon targetWeapon = modifyDamageProperty
  gdElectricity
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE highVoltage1 #-}

highVoltage2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
highVoltage2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE highVoltage2 #-}

-- | High Voltage [+60% Electricity Damage, +60% Status Chance]
highVoltage
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
highVoltage baseWeapon (targetWeapon, mods) =
  ( highVoltage1 baseWeapon $ highVoltage2 baseWeapon targetWeapon
  , "High Voltage [+60% Electricity Damage, +60% Status Chance]" : mods
  )
{-# INLINE highVoltage #-}

-- | Misc. Mods

wildfire1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
wildfire1 baseWeapon targetWeapon = modifyGeneralProperty
  gwMagazineSize
  (baseWeapon ^. gwMagazineSize)
  (Just 0.2)
  (+)
  targetWeapon
{-# INLINE wildfire1 #-}

wildfire2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
wildfire2 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE wildfire2 #-}

-- | Wildfire [+20% Magazine Size, +60% Heat Damage]
wildfire :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
wildfire baseWeapon (targetWeapon, mods) =
  ( wildfire2 baseWeapon $ wildfire1 baseWeapon targetWeapon
  , "Wildfire [+20% Magainze Size, +60% Heat Damage]" : mods
  )
{-# INLINE wildfire #-}
