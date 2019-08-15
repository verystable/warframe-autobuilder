{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.ElementalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify elemental damage, applicable on secondary weapons.

module Builder.Mods.PistolMods.ElementalDamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Elemental Mods

-- | Deep Freeze [+90% Cold Damage]
deepFreeze
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
deepFreeze baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Deep Freeze [+90% Cold Damage]" : mods
  )
{-# INLINE deepFreeze #-}

-- | Primed Deep Freeze [+165% Cold Damage]
primedDeepFreeze
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedDeepFreeze baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Deep Freeze [+165% Cold Damage]" : mods
  )
{-# INLINE primedDeepFreeze #-}

-- | Heated Charge [+90% Heat Damage]
heatedCharge
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
heatedCharge baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Heated Charge [+90% Heat Damage]" : mods
  )
{-# INLINE heatedCharge #-}

-- | Primed Heated Charge [+165% Heat Damage]
primedHeatedCharge
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedHeatedCharge baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Heated Charge [+165% Heat Damage]" : mods
  )
{-# INLINE primedHeatedCharge #-}

-- | Pathogen Rounds [+90% Toxin Damage]
pathogenRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pathogenRounds baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Pathogen Rounds [+90% Toxin Damage]" : mods
  )
{-# INLINE pathogenRounds #-}

-- | Primed Pathogen Rounds [+165% Toxin Damage]
primedPathogenRounds
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedPathogenRounds baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Pathogen Rounds [+165% Toxin Damage]" : mods
  )
{-# INLINE primedPathogenRounds #-}

-- | Convulsion [+90% Electricity Damage]
convulsion
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
convulsion baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Convulsion [+90% Electricity Damage]" : mods
  )
{-# INLINE convulsion #-}

-- | Primed Convulsion [+165% Electricity Damage]
primedConvulsion
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedConvulsion baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Convulsion [+165% Electricity Damage]" : mods
  )
{-# INLINE primedConvulsion #-}

-- | Dual Stat Status Mods

frostbite1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frostbite1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon

frostbite2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frostbite2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon

-- | Frostbite [+60% Cold Damage, +60% Status Chance]
frostbite :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
frostbite baseWeapon (targetWeapon, mods) =
  ( frostbite1 baseWeapon $ frostbite2 baseWeapon targetWeapon
  , "Frostbite [+60% Cold Damage, +60% Status Chance]" : mods
  )
{-# INLINE frostbite1 #-}

scorch1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
scorch1 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon

scorch2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
scorch2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon

-- | Scorch [+60% Heat Damage, +60% Status Chance]
scorch :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
scorch baseWeapon (targetWeapon, mods) =
  ( scorch1 baseWeapon $ scorch2 baseWeapon targetWeapon
  , "Scorch [+60% Heat Damage, +60% Status Chance]" : mods
  )
{-# INLINE scorch1 #-}

pistolPestilence1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
pistolPestilence1 baseWeapon targetWeapon = modifyDamageProperty
  gdToxin
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon

pistolPestilence2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
pistolPestilence2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon

-- | Pistol Pestilence [+60% Toxin Damage, +60% Status Chance]
pistolPestilence
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pistolPestilence baseWeapon (targetWeapon, mods) =
  ( pistolPestilence1 baseWeapon $ pistolPestilence2 baseWeapon targetWeapon
  , "Pistol Pestilence [+60% Toxin Damage, +60% Status Chance]" : mods
  )
{-# INLINE pistolPestilence1 #-}

jolt1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
jolt1 baseWeapon targetWeapon = modifyDamageProperty
  gdElectricity
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE jolt1 #-}

jolt2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
jolt2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE jolt2 #-}

-- | Jolt [+60% Electricity Damage, +60% Status Chance]
jolt :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
jolt baseWeapon (targetWeapon, mods) =
  ( jolt1 baseWeapon $ jolt2 baseWeapon targetWeapon
  , "Jolt [+60% Electricity Damage, +60% Status Chance]" : mods
  )
{-# INLINE jolt #-}

-- | Misc. Mods

iceStorm1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
iceStorm1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE iceStorm1 #-}

iceStorm2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
iceStorm2 baseWeapon targetWeapon = modifyGeneralProperty
  gwMagazineSize
  (targetWeapon ^. gwMagazineSize)
  (Just 0.4)
  (+)
  targetWeapon
{-# INLINE iceStorm2 #-}

-- | Ice Storm [+60% Cold Damage, +40% Magazine Size]
iceStorm :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
iceStorm baseWeapon (targetWeapon, mods) =
  ( iceStorm1 baseWeapon $ iceStorm2 baseWeapon targetWeapon
  , "Ice Storm [+60% Cold Damage, +40% Magazine Size]" : mods
  )
{-# INLINE iceStorm #-}
