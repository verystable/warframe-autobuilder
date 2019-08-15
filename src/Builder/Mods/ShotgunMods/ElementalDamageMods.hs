{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.ElementalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify elemental damage, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.ElementalDamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Elemental Mods

-- | Chilling Grasp [+90% Cold Damage]
chillingGrasp
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
chillingGrasp baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Chilling Grasp [+90% Cold Damage]" : mods
  )
{-# INLINE chillingGrasp #-}

-- | Primed Chilling Grasp [+165% Cold Damage]
primedChillingGrasp
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedChillingGrasp baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Chilling Grasp [+165% Cold Damage]" : mods
  )
{-# INLINE primedChillingGrasp #-}

-- | Incendiary Coat [+90% Heat Damage]
incendiaryCoat
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
incendiaryCoat baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Incendiary Coat [+90% Heat Damage]" : mods
  )
{-# INLINE incendiaryCoat #-}

-- | Primed Incendiary Coat [+165% Heat Damage]
primedIncendiaryCoat
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedIncendiaryCoat baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Incendiary Coat [+165% Heat Damage]" : mods
  )
{-# INLINE primedIncendiaryCoat #-}

-- | Contagious Spread [+90% Toxin Damage]
contagiousSpread
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
contagiousSpread baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Contagious Spread [+90% Toxin Damage]" : mods
  )
{-# INLINE contagiousSpread #-}

-- | Primed Contagious Spread [+165% Toxin Damage]
primedContagiousSpread
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedContagiousSpread baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Contagious Spread [+165% Toxin Damage]" : mods
  )
{-# INLINE primedContagiousSpread #-}

-- | Charged Shell [+90% Electricity Damage]
chargedShell
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
chargedShell baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Charged Shell [+90% Electricity Damage]" : mods
  )
{-# INLINE chargedShell #-}

-- | Primed Charged Shell [+165% Electricity Damage]
primedChargedShell
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedChargedShell baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Charged Shell [+165% Electricity Damage]" : mods
  )
{-# INLINE primedChargedShell #-}


-- | Dual Stat Status Mods

frigidBlast1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frigidBlast1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE frigidBlast1 #-}

frigidBlast2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
frigidBlast2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE frigidBlast2 #-}

-- | Frigid Blast [+60% Cold Damage, +60% Status Chance]
frigidBlast
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
frigidBlast baseWeapon (targetWeapon, mods) =
  ( frigidBlast1 baseWeapon $ frigidBlast2 baseWeapon targetWeapon
  , "Frigid Blast [+60% Cold Damage, +60% Status Chance]" : mods
  )
{-# INLINE frigidBlast #-}

scatteringInferno1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
scatteringInferno1 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE scatteringInferno1 #-}

scatteringInferno2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
scatteringInferno2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE scatteringInferno2 #-}

-- | Scattering Inferno [+60% Heat Damage, +60% Status Chance]
scatteringInferno
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
scatteringInferno baseWeapon (targetWeapon, mods) =
  ( scatteringInferno1 baseWeapon $ scatteringInferno2 baseWeapon targetWeapon
  , "Scattering Inferno [+60% Heat Damage, +60% Status Chance]" : mods
  )
{-# INLINE scatteringInferno #-}

toxicBarrage1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
toxicBarrage1 baseWeapon targetWeapon = modifyDamageProperty
  gdToxin
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE toxicBarrage1 #-}

toxicBarrage2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
toxicBarrage2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE toxicBarrage2 #-}

-- | Toxic Barrage [+60% Toxin Damage, +60% Status Chance]
toxicBarrage
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
toxicBarrage baseWeapon (targetWeapon, mods) =
  ( toxicBarrage1 baseWeapon $ toxicBarrage2 baseWeapon targetWeapon
  , "Toxic Barrage [+60% Toxin Damage, +60% Status Chance]" : mods
  )
{-# INLINE toxicBarrage #-}

shellShock1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
shellShock1 baseWeapon targetWeapon = modifyDamageProperty
  gdElectricity
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE shellShock1 #-}

shellShock2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
shellShock2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE shellShock2 #-}

-- | Shell Shock [+60% Electricity Damage, +60% Status Chance]
shellShock
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shellShock baseWeapon (targetWeapon, mods) =
  ( shellShock1 baseWeapon $ shellShock2 baseWeapon targetWeapon
  , "Shell Shock [+60% Electricity Damage, +60% Status Chance]" : mods
  )
{-# INLINE shellShock #-}

-- | Misc. Mods
blaze1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
blaze1 _ targetWeapon = modifyGeneralProperty gwBaseDamage
                                              (targetWeapon ^. gwBaseDamage)
                                              (Just 0.6)
                                              (+)
                                              targetWeapon
{-# INLINE blaze1 #-}

blaze2 :: GenericWeapon -> GenericWeapon
blaze2 = applyMultiplier (Just 1.6)
{-# INLINE blaze2 #-}

blaze3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
blaze3 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE blaze3 #-}

-- | Blaze [+60% Damage, +60% Heat Damage]
blaze :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
blaze baseWeapon (targetWeapon, mods) =
  ( blaze3 baseWeapon $ blaze2 $ blaze1 baseWeapon targetWeapon
  , "Blaze [+60% Damage, +60% Heat Daage]" : mods
  )
{-# INLINE blaze #-}

chillingReload1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
chillingReload1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE chillingReload1 #-}

chillingReload2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
chillingReload2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (targetWeapon ^. gwReloadTime)
  (Just 0.4)
  (-)
  targetWeapon
{-# INLINE chillingReload2 #-}

-- | Chilling Reload [+60% Cold Damage, -40% Reload Time]
chillingReload
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
chillingReload baseWeapon (targetWeapon, mods) =
  ( chillingReload1 baseWeapon $ chillingReload2 baseWeapon targetWeapon
  , "Chilling Reload [+60% Cold Damage, -40% Reload Time]" : mods
  )
{-# INLINE chillingReload #-}
