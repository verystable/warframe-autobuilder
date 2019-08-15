{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.DamageMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify damage, applicable on melee weapons.

module Builder.Mods.MeleeMods.DamageMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

pressurePoint1 :: GenericWeapon -> GenericWeapon
pressurePoint1 targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.2)
  (+)
  targetWeapon
{-# INLINE pressurePoint1 #-}

pressurePoint2 :: GenericWeapon -> GenericWeapon
pressurePoint2 gw = applyMultiplier (Just 2.2) gw
{-# INLINE pressurePoint2 #-}

-- | Pressure Point [+120% Damage]
pressurePoint
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pressurePoint baseWeapon (targetWeapon, mods) =
  ( pressurePoint1 $ pressurePoint2 targetWeapon
  , "Pressure Point [+120% Damage]" : mods
  )
{-# INLINE pressurePoint #-}

primedPressurePoint1 :: GenericWeapon -> GenericWeapon
primedPressurePoint1 targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.65)
  (+)
  targetWeapon
{-# INLINE primedPressurePoint1 #-}

primedPressurePoint2 :: GenericWeapon -> GenericWeapon
primedPressurePoint2 = applyMultiplier (Just 2.65)
{-# INLINE primedPressurePoint2 #-}

-- | Primed Pressure Point [+165% Damage]
primedPressurePoint
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedPressurePoint baseWeapon (targetWeapon, mods) =
  ( primedPressurePoint1 $ primedPressurePoint2 targetWeapon
  , "Primed Pressure Point [+165% Damage]" : mods
  )
{-# INLINE primedPressurePoint #-}

sacrificialPressure1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
sacrificialPressure1 _ targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.1)
  (+)
  targetWeapon
{-# INLINE sacrificialPressure1 #-}

sacrificialPressure2 :: GenericWeapon -> GenericWeapon
sacrificialPressure2 = applyMultiplier (Just 2.1)
{-# INLINE sacrificialPressure2 #-}

-- | Sacrificial Pressure [+110% Damage, +33% Damage to Sentients]
sacrificialPressure
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sacrificialPressure baseWeapon (targetWeapon, mods) =
  ( sacrificialPressure1 baseWeapon $ sacrificialPressure2 targetWeapon
  , "Sacrificial Pressure [+110% Damage, +33% Damage to Sentients]" : mods
  )
{-# INLINE sacrificialPressure #-}

spoiledStrike1 :: GenericWeapon -> GenericWeapon
spoiledStrike1 targetWeapon = modifyGeneralProperty
  gwBaseDamage
  (targetWeapon ^. gwBaseDamage)
  (Just 1.2)
  (+)
  targetWeapon
{-# INLINE spoiledStrike1 #-}

spoiledStrike2 :: GenericWeapon -> GenericWeapon
spoiledStrike2 = applyMultiplier (Just 2.2)
{-# INLINE spoiledStrike2 #-}

spoiledStrike3 :: GenericWeapon -> GenericWeapon -> GenericWeapon
spoiledStrike3 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.2)
  (-)
  targetWeapon
{-# INLINE spoiledStrike3 #-}

-- | Spoiled Strike [+100% Damage, -20% Attack Speed]
spoiledStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
spoiledStrike baseWeapon (targetWeapon, mods) =
  ( spoiledStrike1 $ spoiledStrike2 $ spoiledStrike3 baseWeapon targetWeapon
  , "Spoiled Strike [+100% Damage, -20% Attack Speed]" : mods
  )
{-# INLINE spoiledStrike #-}

-- | Condition Overload [+60% Melee Damage for each status type on target]
conditionOverload
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
conditionOverload baseWeapon (targetWeapon, mods) =
  ( targetWeapon
  , "Condition Overload [+60% Melee Damage for each status type on target]"
    : mods
  )
{-# INLINE conditionOverload #-}
