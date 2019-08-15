{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.ElementalDamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify elemental damage, applicable on melee weapons.

module Builder.Mods.MeleeMods.ElementalDamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Elemental Mods

-- | North Wind [+90% Cold Damage]
northWind :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
northWind baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "North Wind [+90% Cold Damage]" : mods
  )
{-# INLINE northWind #-}

-- | Primed North Wind [+165% Cold Damage]
primedNorthWind
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedNorthWind baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdCold
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed North Wind [+165% Cold Damage]" : mods
  )
{-# INLINE primedNorthWind #-}

-- | Molten Impact [+90% Heat Damage]
moltenImpact
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
moltenImpact baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Molten Impact [+90% Heat Damage]" : mods
  )
{-# INLINE moltenImpact #-}

-- | Primed Molten Impact [+165% Heat Damage]
primedMoltenImpact
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedMoltenImpact baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdHeat
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Molten Impact [+165% Heat Damage]" : mods
  )
{-# INLINE primedMoltenImpact #-}

-- | Fever Strike [+90% Toxin Damage]
feverStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
feverStrike baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Fever Strike [+90% Toxin Damage]" : mods
  )
{-# INLINE feverStrike #-}

-- | Primed Fever Strike [+165% Toxin Damage]
primedFeverStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedFeverStrike baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdToxin
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Fever Strike [+165% Toxin Damage]" : mods
  )
{-# INLINE primedFeverStrike #-}

-- | Shocking Touch [+90% Electricity Damage]
shockingTouch
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shockingTouch baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 0.9)
                         (+)
                         targetWeapon
  , "Shocking Touch [+90% Electricity Damage]" : mods
  )
{-# INLINE shockingTouch #-}

-- | Primed Shocking Touch [+165% Electricity Damage]
primedShockingTouch
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedShockingTouch baseWeapon (targetWeapon, mods) =
  ( modifyDamageProperty gdElectricity
                         (targetWeapon ^. gwBaseDamage)
                         (Just 1.65)
                         (+)
                         targetWeapon
  , "Primed Shocking Touch [+165% Electricity Damage]" : mods
  )
{-# INLINE primedShockingTouch #-}

-- | Dual Stat Status Mods

viciousFrost1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
viciousFrost1 baseWeapon targetWeapon = modifyDamageProperty
  gdCold
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE viciousFrost1 #-}

viciousFrost2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
viciousFrost2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE viciousFrost2 #-}

-- | Vicious Frost [+60% Cold Damage, +60% Status Chance]
viciousFrost
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
viciousFrost baseWeapon (targetWeapon, mods) =
  ( viciousFrost1 baseWeapon $ viciousFrost2 baseWeapon targetWeapon
  , "Vicious Frost [+60% Cold Damage, +60% Status Chance]" : mods
  )
{-# INLINE viciousFrost #-}

vocanicEdge1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
vocanicEdge1 baseWeapon targetWeapon = modifyDamageProperty
  gdHeat
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon

vocanicEdge2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
vocanicEdge2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon

-- | Vocanic Edge [+60% Heat Damage, +60% Status Chance]
vocanicEdge
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
vocanicEdge baseWeapon (targetWeapon, mods) =
  ( vocanicEdge1 baseWeapon $ vocanicEdge2 baseWeapon targetWeapon
  , "Vocanic Edge [+60% Heat Damage, +60% Status Chance]" : mods
  )
{-# INLINE vocanicEdge #-}

virulentScourge1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
virulentScourge1 baseWeapon targetWeapon = modifyDamageProperty
  gdToxin
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE virulentScourge1 #-}

virulentScourge2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
virulentScourge2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE virulentScourge2 #-}

-- | Virulent Scourge [+60% Toxin Damage, +60% Status Chance]
virulentScourge
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
virulentScourge baseWeapon (targetWeapon, mods) =
  ( virulentScourge1 baseWeapon $ virulentScourge2 baseWeapon targetWeapon
  , "Virulent Scourge [+60% Toxin Damage, +60% Status Chance]" : mods
  )
{-# INLINE virulentScourge #-}

voltaicStrike1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
voltaicStrike1 baseWeapon targetWeapon = modifyDamageProperty
  gdElectricity
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE voltaicStrike1 #-}

voltaicStrike2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
voltaicStrike2 baseWeapon targetWeapon = modifyGeneralProperty
  gwStatusChance
  (baseWeapon ^. gwStatusChance)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE voltaicStrike2 #-}

-- | Voltaic Strike [+60% Electricity Damage, +60% Status Chance]
voltaicStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
voltaicStrike baseWeapon (targetWeapon, mods) =
  ( voltaicStrike1 baseWeapon $ voltaicStrike2 baseWeapon targetWeapon
  , "Voltaic Strike [+60% Electricity Damage, +60% Status Chance]" : mods
  )
{-# INLINE voltaicStrike #-}

-- | Misc. Mods

focusEnergy1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
focusEnergy1 baseWeapon targetWeapon = modifyDamageProperty
  gdElectricity
  (targetWeapon ^. gwBaseDamage)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE focusEnergy1 #-}

focusEnergy2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
focusEnergy2 baseWeapon targetWeapon = modifyGeneralProperty
  gwChannelingCost
  (targetWeapon ^. gwChannelingCost)
  (Just 0.4)
  (+)
  targetWeapon
{-# INLINE focusEnergy2 #-}

-- | Focus Energy [+60% Electricity Damage, +40% Channeling Efficiency]
focusEnergy
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
focusEnergy baseWeapon (targetWeapon, mods) =
  ( focusEnergy1 baseWeapon $ focusEnergy2 baseWeapon targetWeapon
  , "Focus Energy [+60% Electricity Damage, +40% Channeling Efficiency]" : mods
  )
{-# INLINE focusEnergy #-}
